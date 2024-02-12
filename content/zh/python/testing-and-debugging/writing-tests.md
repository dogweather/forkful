---
title:                "编写测试"
aliases: - /zh/python/writing-tests.md
date:                  2024-02-03T19:31:33.046068-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Python 中编写测试涉及创建自动化脚本来验证代码的正确性。程序员这样做是为了确保他们的函数或类在各种条件下按预期工作，这有助于早期发现错误并促进更容易的维护和重构。

## 如何操作：
Python 自带了一个用于编写测试的内置模块，名为 `unittest`。这是使用它来测试一个简单函数的方法：

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "应该是 12")

if __name__ == '__main__':
    unittest.main()
```

当你运行这个测试脚本时，你应该会看到输出，指示你的测试通过（或失败）。

若要进行更现代和更具表达性的测试，你可以使用第三方库，如 `pytest`。首先，你需要使用 pip 安装它：

```shell
pip install pytest
```

然后，你可以以更简单的方式编写你的测试，无需继承任何内容：

```python
# 将此保存在一个名为 test_with_pytest.py 的文件中
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "应该是 12"
```

要使用 `pytest` 运行你的测试，只需执行：

```shell
pytest test_with_pytest.py
```

你应该会看到 pytest 显示的测试结果输出。
