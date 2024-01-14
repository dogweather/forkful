---
title:    "Python: 「テストの作成」"
keywords: ["Python"]
---

{{< edit_this_page >}}

## なぜテストを書くのか

テストを書くことによって、コードの信頼性と品質を向上させることができます。テストはバグを見つけるだけでなく、コードの変更によって意図しない副作用が起きていないかをチェックすることもできます。また、テストを書くことで複雑なコードを理解しやすくなることもあります。

## テストを書く方法

テストを書く際の基本的な手順は以下の通りです。

- `unittest`をインポートする
- テスト用のクラスを作成する（`class TestClass(unittest.TestCase):`）
- `setUp()`メソッド内でテスト用のデータを準備する
- テスト用のメソッドを作成する（`test_something()`）
- `assert`文を使って実際の結果と期待する結果が一致するかを確認する
- `tearDown()`メソッド内でテスト用のデータをクリーンアップする

下記の例では、`Calculator`クラスの`add()`メソッドをテストしています。実際にコードを実行して、アサーションエラーが起きないことを確認してください。

```Python
import unittest

class Calculator:
    def add(self, a, b):
        return a + b

class TestCalculator(unittest.TestCase):
    def setUp(self):
        self.calculator = Calculator()

    def test_add(self):
        result = self.calculator.add(5, 10)
        self.assertEqual(result, 15)

if __name__ == "__main__":
    unittest.main()
```

## テストを深堀りする

テストについてはさまざまなトピックがありますが、ここでは3つの重要なポイントを紹介します。

- テストカバレッジを意識することで、コードのあらゆる部分をカバーするテストを書くことができます。
- リファクタリングを行った際に、テストが通ることを保証することで、コードの改善を安全に行うことができます。
- テストダブル（モックやスタブなど）を使うことで、外部リソースに依存する場合でもテストを実行することができます。

以上がテストを書く上で重要なポイントですが、より詳細な情報を得るためには、さらに学習する必要があります。

## さらに見る

テストについてはまだまだ語り尽くせるほどの情報があります。以下のリンクを参考に、さらに深く学習してみてください。

- [unittestモジュール公式ドキュメント (英語)](https://docs.python.org/ja/3/library/unittest.html)
- [Pythonにおけるテストカバレッジの重要性 (日本語)](https://codezine.jp/article/detail/10773)
- [リファクタリング中のテスト駆動開発 (英語)](https://medium.com/@webprolific/getting-started-with-tdd-in-python-2579ef4450e3)