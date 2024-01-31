---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
编写测试即是为代码创建一系列检验，确保其按预期运行。程序员这么做是为了提前发现错误，保证软件的质量和可靠性。

## How to: (如何操作)
以下示例使用Python内置的`unittest`模块：

```Python
import unittest

def add(x, y):
    return x + y

class TestAddition(unittest.TestCase):
    def test_add_integers(self):
        self.assertEqual(add(3, 4), 7)

    def test_add_strings(self):
        self.assertEqual(add("Hello", "World"), "HelloWorld")

if __name__ == '__main__':
    unittest.main()
```

运行后输出:
```
..
----------------------------------------------------------------------
Ran 2 tests in 0.001s

OK
```

## Deep Dive (深度探索)
- 历史背景: 测试驱动开发（TDD）自2000年起变得流行，强调先写测试再写功能代码。
- 替代方案: 除了`unittest`，还有`pytest`、`nose2`等测试框架。
- 实现细节: `unittest`支持测试固件、测试用例、测试套件和测试运行器的概念，专业地组织和运行测试。

## See Also (另请参见)
- 官方unittest文档: https://docs.python.org/3/library/unittest.html
- pytest官网: https://pytest.org/
- 测试驱动开发(TDD): https://en.wikipedia.org/wiki/Test-driven_development
