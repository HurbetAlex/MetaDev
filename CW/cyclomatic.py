import re
import sys

def count_cyclomatic_complexity(file_path):
    with open(file_path, 'r') as file:
        content = file.read()

    function_pattern = r'def\s+(\w+)\s*\([^)]*\)\s*:\s*(.*?)(?=def\s+|\Z)'
    functions = re.findall(function_pattern, content, re.DOTALL)

    for func_name, func_body in functions:
        complexity = 1

        patterns = [
            r'\bif\b', r'\belif\b', r'\bfor\b', r'\bwhile\b',
            r'\bdo\b', r'\bcase\b', r'\btry\b', r'\bexcept\b',
            r'\bfinally\b', r'\bwith\b'
        ]
        ternary_pattern = r'\?\s*[^:]+:\s*[^)]+'
        logical_operators_pattern = r'\s*(and|or)\s*'

        for pattern in patterns:
            matches = re.findall(pattern, func_body)
            complexity += len(matches)

        ternary_matches = re.findall(ternary_pattern, func_body)
        complexity += len(ternary_matches)

        logical_operator_matches = re.findall(logical_operators_pattern, func_body)
        complexity += len(logical_operator_matches)

        print(f"{func_name} ({complexity})")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python cyclomatic.py <file_path>")
        sys.exit(1)

    file_path = sys.argv[1]
    count_cyclomatic_complexity(file_path)