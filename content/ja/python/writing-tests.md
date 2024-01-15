---
title:                "テストの書き方"
html_title:           "Python: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ
テストを書くことは、ソフトウェア開発のプロセスにおいて非常に重要です。ソフトウェアをデバッグし、バグを見つけるのに役立ちます。

## 作り方
テストを書くには、まずテストフレームワークをインストールする必要があります。Pythonでは、標準ライブラリの`unittest`やサードパーティの`pytest`が一般的に使われています。

```Python
# unittestを使った例
import unittest

# テスト用の関数
def add(a, b):
    return a + b

# テストクラス
class TestAdd(unittest.TestCase):
    
    def test_add(self):
        self.assertEqual(add(2, 3), 5)

# テストを実行する
if __name__ == '__main__':
    unittest.main()

# pytestを使った例
# test_add.pyというファイルを作成し、以下のコードを書く
def add(a, b):
    return a + b

# テスト用の関数
def test_add():
    assert add(2, 3) == 5

# コマンドラインでpytestコマンドを実行するとテストが実行される
```

上記の例では、`add()`関数に対して`5`が返されるかどうかをテストしています。`unittest`では`TestCase`クラスを継承し、`assertEqual()`メソッドを使って結果を検証し、`pytest`では`assert`キーワードを使って同様の操作を行います。

## ディープダイブ
テストを書くことで、ソフトウェアの誤動作や問題を早期に発見することができ、バグの修正にかかる時間やコストを減らすことができます。また、テストコード自体がドキュメントとなり、ソフトウェアの挙動を理解するのに役立ちます。

また、テストを書く際には以下のようなことに気をつけることが重要です。

- テストの範囲を正しく決める
  - 全てのケースを網羅するのではなく、重要なケースをテストすることに重点を置く
- テストコードもメンテナンスする
  - 試験前・試験後の状態を整えるためのコードをきちんと管理する

## 関連情報
- [Python標準ライブラリ - unittest](https://docs.python.org/ja/3/library/unittest.html)
- [pytest documentation](https://docs.pytest.org/en/latest/)