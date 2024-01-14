---
title:    "Arduino: テストの書き方"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜテストを書くべきか

Arduinoプログラミングを行う際に、あなたはテストを書くことを考えたことがありますか？テストは、あなたが書いたコードが正しく動作するかどうかを確認するための重要なステップです。確実に動作するコードを書くためには、テストが欠かせません。

## テストの書き方

テストを書くには、Arduinoが提供するテスト用ライブラリを使用します。例として、以下のようなコードを書いてみましょう。

```Arduino
#include <ArduinoUnit.h>

unittest(testName) {
  String message = "Hello World";
  assertEqual(message,"Hello World");
}
```

上記のコードでは、 `unittest` という命令を使ってテストを行っています。テストで使用される名前は `unittest` の後に続けて記述します。次に、 `assertEqual` 命令を使用して、 `message` が "Hello World" と等しいことをテストしています。

テストを実行すると、以下のような結果が返ってきます。

```
Test: testName
PASS

OK (1 of 1 tests passed)
```

テストが正常に動作したことが確認できました。

## テストの詳細

テストを書く際には、いくつかの注意点があります。まず、テストはコードの各部分を独立してテストする必要があります。また、テストが想定した通りに動作しない場合は、問題のあるコードを特定するように努める必要があります。

さらに、テストはあなたのコードが変更された際にも常に正しく動作することを確認するためにも重要です。コードが変更された場合は、テストを再実行して問題がないかどうかを確認しましょう。

## あわせて読みたい

- [Arduinoテスト用ライブラリのドキュメント (英語)](https://github.com/mmurdoch/arduinounit/)
- [テスト自動化入門 (日本語)](https://thinkit.co.jp/story/2013/08/07/4412)
- [テスト駆動開発入門 (日本語)](https://codezine.jp/article/detail/627)