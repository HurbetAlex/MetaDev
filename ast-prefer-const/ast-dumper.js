// ast-dumper.js
let fs = require("fs");
const path = require("path");
const acorn = require("acorn");
const commander = require("commander");

// Set up command line interface
const program = new commander.Command();
program
  .name("ast-dumper")
  .description("Dump AST of a JavaScript file as JSON")
  .version("1.0.0")
  .argument("<file>", "JavaScript file to parse")
  .option("-o, --output <file>", "Output file (defaults to stdout)")
  .parse(process.argv);

const options = program.opts();
const filePath = program.args[0];

// Check if file exists
if (!fs.existsSync(filePath)) {
  console.error(`Error: File "${filePath}" does not exist`);
  process.exit(1);
}

// Read the file
try {
  const code = fs.readFileSync(filePath, "utf8");

  // Parse the code to get AST
  const ast = acorn.parse(code, {
    ecmaVersion: "latest",
    sourceType: "module",
    locations: true,
    ranges: true,
    onComment: [],
  });

  // Convert AST to formatted JSON
  const jsonOutput = JSON.stringify(ast, null, 2);

  // Output AST as JSON
  if (options.output) {
    fs.writeFileSync(options.output, jsonOutput);
    console.log(`AST dumped to ${options.output}`);
  } else {
    console.log(jsonOutput);
  }
} catch (error) {
  console.error(`Error parsing file: ${error.message}`);
  process.exit(1);
}
