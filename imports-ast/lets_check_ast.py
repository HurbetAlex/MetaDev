import ast
import os
import sys
from pathlib import Path


def extract_imports(file_path):
    imports = []
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            tree = ast.parse(f.read(), filename=str(file_path))

        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append(alias.name)
            elif isinstance(node, ast.ImportFrom):
                if node.module:
                    imports.append(node.module)

    except SyntaxError:
        print(f"Ошибка синтаксиса в {file_path}, пропускаем.")

    return imports


def analyze_directory(directory):
    base_dir = Path(directory).resolve()
    dependencies = {}

    for root, _, files in os.walk(base_dir):
        if ".venv" in root or "__pycache__" in root:
            continue

        for file in files:
            if file.endswith(".py"):
                file_path = Path(root) / file
                relative_file_path = file_path.relative_to(base_dir)
                imports = extract_imports(file_path)
                dependencies[str(relative_file_path)] = imports

    return dependencies


def generate_mermaid(dependencies):
    print("graph LR;")
    for file, imports in dependencies.items():
        file_node = Path(file).name
        for imp in imports:
            imp_node = imp.replace(".", "/") + ".py"
            print(f"  {file_node} --> {imp_node}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python lets_check_ast.py <directory>")
        sys.exit(1)

    project_dir = sys.argv[1]
    if not os.path.isdir(project_dir):
        print("Error: Directory does not exist")
        sys.exit(1)

    dependencies = analyze_directory(project_dir)
    generate_mermaid(dependencies)
