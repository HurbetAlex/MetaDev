import os
import re
import sys
from pathlib import Path


def extract_imports(file_path, base_dir):
    imports = []
    import_pattern = re.compile(r'^\s*(?:from|import)\s+([\w\.]+)', re.MULTILINE)

    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
        matches = import_pattern.findall(content)

        for module in matches:
            module_path = module.replace('.', '/')
            possible_paths = [
                file_path.parent / f"{module}.py",
                # Path(base_dir) / f"{module}.py", Якщо потрібно з папки вище
                Path(base_dir) / f"{module_path}.py",
                Path(base_dir) / f"{module_path}/__init__.py"
            ]

            for path in possible_paths:
                if path.exists():
                    relative_path = path.relative_to(base_dir)
                    imports.append(str(relative_path))
                    break

    return imports







def analyze_directory(directory):
    base_dir = Path(directory).resolve()
    dependencies = {}

    for root, _, files in os.walk(base_dir):
        if '.venv' in root or 'pycache' in root:
            continue

        for file in files:
            if file.endswith('.py'):
                file_path = Path(root) / file
                relative_file_path = file_path.relative_to(base_dir)
                imports = extract_imports(file_path, base_dir)
                dependencies[str(relative_file_path)] = imports

    return dependencies



def generate_mermaid(dependencies):
    print("graph LR;")
    for file, imports in dependencies.items():
        file_node = Path(file).name
        for imp in imports:
            imp_node = imp.replace(os.sep, '/')
            print(f"  {file_node} --> {imp_node}")


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python analyze_imports.py <directory>")
        sys.exit(1)

    project_dir = sys.argv[1]
    if not os.path.isdir(project_dir):
        print("Error: Directory does not exist")
        sys.exit(1)

    dependencies = analyze_directory(project_dir)
    generate_mermaid(dependencies)