---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:33.046068-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Python \u81EA\u5E26\u4E86\u4E00\u4E2A\
  \u7528\u4E8E\u7F16\u5199\u6D4B\u8BD5\u7684\u5185\u7F6E\u6A21\u5757\uFF0C\u540D\u4E3A\
  \ `unittest`\u3002\u8FD9\u662F\u4F7F\u7528\u5B83\u6765\u6D4B\u8BD5\u4E00\u4E2A\u7B80\
  \u5355\u51FD\u6570\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:46.439371-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Python \u81EA\u5E26\u4E86\u4E00\u4E2A\u7528\
  \u4E8E\u7F16\u5199\u6D4B\u8BD5\u7684\u5185\u7F6E\u6A21\u5757\uFF0C\u540D\u4E3A `unittest`\u3002\
  \u8FD9\u662F\u4F7F\u7528\u5B83\u6765\u6D4B\u8BD5\u4E00\u4E2A\u7B80\u5355\u51FD\u6570\
  \u7684\u65B9\u6CD5\uFF1A."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

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
