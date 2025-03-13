import time
import functools
import ast
import inspect

class MagicTransformer(ast.NodeTransformer):
    def insert_cache(self, node: ast.FunctionDef) -> None:
        cache_key = ast.Tuple(
            elts=[ast.Name(id="args", ctx=ast.Load()), ast.Name(id="kwargs", ctx=ast.Load())],
            ctx=ast.Load()
        )
        cache_check = ast.If(
            test=ast.Compare(
                left=cache_key,
                ops=[ast.In()],
                comparators=[ast.Name(id="_magic_cache", ctx=ast.Load())]
            ),
            body=[
                ast.Return(
                    value=ast.Subscript(
                        value=ast.Name(id="_magic_cache", ctx=ast.Load()),
                        slice=cache_key,
                        ctx=ast.Load()
                    )
                )
            ],
            orelse=[]
        )
        cache_store = ast.Assign(
            targets=[
                ast.Subscript(
                    value=ast.Name(id="_magic_cache", ctx=ast.Load()),
                    slice=cache_key,
                    ctx=ast.Store()
                )
            ],
            value=ast.Name(id="return_value", ctx=ast.Load())
        )
        node.body.insert(0, cache_check)
        node.body.append(cache_store)

    def insert_type_check(self, node: ast.FunctionDef) -> None:
        for arg in node.args.args:
            if arg.annotation:
                node.body.insert(0, ast.If(
                    test=ast.Compare(
                        left=ast.Call(
                            func=ast.Name(id="type", ctx=ast.Load()),
                            args=[ast.Name(id=arg.arg, ctx=ast.Load())],
                            keywords=[]
                        ),
                        ops=[ast.NotEq()],
                        comparators=[arg.annotation]
                    ),
                    body=[ast.Raise(
                        exc=ast.Call(
                            func=ast.Name(id="TypeError", ctx=ast.Load()),
                            args=[ast.Constant(value=f"Expected {arg.annotation} but got {type(arg.arg)}")],
                            keywords=[]
                        ),
                        cause=None
                    )],
                ))

    def visit_FunctionDef(self, node: ast.FunctionDef) -> ast.FunctionDef:
        node = self.generic_visit(node)
        node.decorator_list = [decorator for decorator in node.decorator_list if decorator.id != "magic"]
        self.insert_type_check(node)
        self.insert_cache(node)
        return node

def apply_magic(func: callable) -> callable:
    source = inspect.getsource(func)
    tree = ast.parse(source)
    new_tree = MagicTransformer().visit(tree)
    ast.fix_missing_locations(new_tree)
    exec(compile(new_tree, filename="<ast>", mode="exec"), globals())
    return globals()[func.__name__]

_magic_cache = {}

def magic(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        cache_key = (func.__name__, args, frozenset(kwargs.items()))
        if cache_key in _magic_cache:
            return _magic_cache[cache_key]
        return_value = func(*args, **kwargs)
        _magic_cache[cache_key] = return_value
        return return_value
    wrapper = apply_magic(wrapper)
    return wrapper

@magic
def fibonacci(n: int) -> int:
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

start_time = time.time()
print(fibonacci(35))
print("--- %s seconds ---" % (time.time() - start_time))
start_time = time.time()
print(fibonacci(35))
print("--- %s seconds ---" % (time.time() - start_time))
