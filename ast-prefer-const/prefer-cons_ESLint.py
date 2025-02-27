import json
import sys

def load_ast(file_path):
    with open(file_path, 'r', encoding="utf-8") as f:
        return json.load(f)

def  find_let_declarations(ast):
    let_declarations = {}

    def traverse(node, parent=None):
        if isinstance(node, dict):
            if node.get("type") == "VariableDeclaration" and node.get("kind") == "let":
                for decl in node.get("declarations", []):
                    if decl.get("id") and "name" in decl["id"]:
                        name = decl["id"]["name"]
                        line = decl["loc"]["start"]["line"]
                        let_declarations[name] = {"line": line, "modified": False}

            for key in node:
                traverse(node[key], node)
        elif isinstance(node, list):
            for item in node:
                traverse(item, parent)

    traverse(ast)
    return let_declarations

def find_variable_modifications(ast, let_declarations):
    def traverse(node):
        if isinstance(node, dict):
            if node.get("type") == "AssignmentExpression":
                if "name" in node.get("left", {}):
                    name = node["left"]["name"]
                    if name in let_declarations:
                        let_declarations[name]["modified"] = True

            for key in node:
                traverse(node[key])
        elif isinstance(node, list):
            for item in node:
                traverse(item)

    traverse(ast)

def suggest_const_replacements(let_declarations):
    for name, info in let_declarations.items():
        if not info["modified"]:
            print(f"Identifier {name} at line {info["line"]} should be const")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python prefer-cons_ESLint.py example.json")
        sys.exit(1)

    ast_file = sys.argv[1]
    ast = load_ast(ast_file)
    let_declarations = find_let_declarations(ast)
    find_variable_modifications(ast, let_declarations)
    suggest_const_replacements(let_declarations)