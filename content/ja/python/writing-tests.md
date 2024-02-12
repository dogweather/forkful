---
title:                "テストの作成"
aliases:
- ja/python/writing-tests.md
date:                  2024-02-03T19:31:33.784791-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
