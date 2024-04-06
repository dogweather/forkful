---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:33.784791-07:00
description: "\u65B9\u6CD5\uFF1A Python\u306B\u306F`unittest`\u3068\u3044\u3046\u30C6\
  \u30B9\u30C8\u3092\u8A18\u8FF0\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\
  \u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059\u3002\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u3066\u30B7\u30F3\u30D7\u30EB\u306A\u95A2\u6570\
  \u3092\u30C6\u30B9\u30C8\u3059\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u3068\u304A\u308A\
  \u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.839943-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Python\u306B\u306F`unittest`\u3068\u3044\u3046\u30C6\u30B9\
  \u30C8\u3092\u8A18\u8FF0\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u304C\u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059\u3002\u3053\
  \u308C\u3092\u4F7F\u7528\u3057\u3066\u30B7\u30F3\u30D7\u30EB\u306A\u95A2\u6570\u3092\
  \u30C6\u30B9\u30C8\u3059\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u3068\u304A\u308A\u3067\
  \u3059\uFF1A."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 方法：
Pythonには`unittest`というテストを記述するための組み込みモジュールが付属しています。これを使用してシンプルな関数をテストする方法は次のとおりです：

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "12であるべき")

if __name__ == '__main__':
    unittest.main()
```

このテストスクリプトを実行すると、テストが通過したか（または失敗したか）を示す出力を見ることができます。

よりモダンで表現力豊かなテストを行うには、`pytest`のようなサードパーティのライブラリを使用できます。まず、pipを使用してインストールする必要があります：

```shell
pip install pytest
```

その後、何もサブクラス化する必要なく、よりシンプルにテストを記述できます：

```python
# test_with_pytest.pyという名前のファイルにこれを保存
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "12であるべき"
```

`pytest`を使用してテストを実行するには、単に実行してください：

```shell
pytest test_with_pytest.py
```

pytestからテスト結果を示す出力を見ることができるでしょう。
