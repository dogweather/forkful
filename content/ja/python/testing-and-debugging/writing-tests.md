---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:33.784791-07:00
description: "Python\u3067\u30C6\u30B9\u30C8\u3092\u8A18\u8FF0\u3059\u308B\u3068\u306F\
  \u3001\u30B3\u30FC\u30C9\u306E\u6B63\u3057\u3055\u3092\u691C\u8A3C\u3059\u308B\u81EA\
  \u52D5\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u69D8\u3005\u306A\u6761\u4EF6\u4E0B\
  \u3067\u305D\u306E\u95A2\u6570\u3084\u30AF\u30E9\u30B9\u304C\u671F\u5F85\u901A\u308A\
  \u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u4FDD\u8A3C\u3057\u3001\u30A8\u30E9\
  \u30FC\u3092\u65E9\u671F\u306B\u767A\u898B\u3057\u3001\u4FDD\u5B88\u3084\u30EA\u30D5\
  \u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.776280
model: gpt-4-0125-preview
summary: "Python\u3067\u30C6\u30B9\u30C8\u3092\u8A18\u8FF0\u3059\u308B\u3068\u306F\
  \u3001\u30B3\u30FC\u30C9\u306E\u6B63\u3057\u3055\u3092\u691C\u8A3C\u3059\u308B\u81EA\
  \u52D5\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u69D8\u3005\u306A\u6761\u4EF6\u4E0B\
  \u3067\u305D\u306E\u95A2\u6570\u3084\u30AF\u30E9\u30B9\u304C\u671F\u5F85\u901A\u308A\
  \u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u4FDD\u8A3C\u3057\u3001\u30A8\u30E9\
  \u30FC\u3092\u65E9\u671F\u306B\u767A\u898B\u3057\u3001\u4FDD\u5B88\u3084\u30EA\u30D5\
  \u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何を、なぜ？
Pythonでテストを記述するとは、コードの正しさを検証する自動スクリプトを作成することを意味します。プログラマーはこれを行うことで、様々な条件下でその関数やクラスが期待通りに動作することを保証し、エラーを早期に発見し、保守やリファクタリングを容易にします。

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
