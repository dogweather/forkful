---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:33.046068-07:00
description: "\u5728 Python \u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\
  \u81EA\u52A8\u5316\u811A\u672C\u6765\u9A8C\u8BC1\u4EE3\u7801\u7684\u6B63\u786E\u6027\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\
  \u7684\u51FD\u6570\u6216\u7C7B\u5728\u5404\u79CD\u6761\u4EF6\u4E0B\u6309\u9884\u671F\
  \u5DE5\u4F5C\uFF0C\u8FD9\u6709\u52A9\u4E8E\u65E9\u671F\u53D1\u73B0\u9519\u8BEF\u5E76\
  \u4FC3\u8FDB\u66F4\u5BB9\u6613\u7684\u7EF4\u62A4\u548C\u91CD\u6784\u3002"
lastmod: '2024-03-13T22:44:47.259657-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Python \u4E2D\u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\u81EA\
  \u52A8\u5316\u811A\u672C\u6765\u9A8C\u8BC1\u4EE3\u7801\u7684\u6B63\u786E\u6027\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\
  \u51FD\u6570\u6216\u7C7B\u5728\u5404\u79CD\u6761\u4EF6\u4E0B\u6309\u9884\u671F\u5DE5\
  \u4F5C\uFF0C\u8FD9\u6709\u52A9\u4E8E\u65E9\u671F\u53D1\u73B0\u9519\u8BEF\u5E76\u4FC3\
  \u8FDB\u66F4\u5BB9\u6613\u7684\u7EF4\u62A4\u548C\u91CD\u6784\u3002."
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
