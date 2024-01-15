---
title:                "コンピュータプログラミングの記事のタイトルは「テストを書く」です。"
html_title:           "Gleam: コンピュータプログラミングの記事のタイトルは「テストを書く」です。"
simple_title:         "コンピュータプログラミングの記事のタイトルは「テストを書く」です。"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書く必要があるのか 

テストを書くことは、コードが正しく動作し、新しい機能を追加したり、リファクタリングしたりする際に自信を持って続けることができるようにするためです。

## 方法 

テストを書くには、まずテストするコードの関数やモジュールを作成します。その後、作成した関数やモジュールを呼び出して、正しい出力が得られるかどうかをテストします。具体的なコーディング例を見てみましょう。

``` Gleam
import string
test suite "String Library Tests" {
  test "split" {
    assert.equal(string.split("Hello World", ""), ["Hello", "World"])
  }
  test "trim" {
    assert.equal(string.trim("   Hello World   "), "Hello World")
  }
}
```

ここでは、`import` ローカルで `test suite` が作成され、2 つの単体テストが実行されています。 `string.split` が正しい出力を返し、 `string.trim` が余分なスペースを削除していることを確認しています。

## 深堀り 

テストを書く際には、さまざまな機能を使用して、コードの異なる部分をテストすることができます。例えば、 `assert.equal` を使用して値が等しいかどうかをテストしたり、 `assert.is_error` を使用してエラーが発生するかどうかをテストしたりすることができます。また、 `setup` 関数を使用して、テストの前に実行する前処理を定義することもできます。

## See Also 

- 公式のGleamドキュメント：https://gleam.run/
- テストの例：https://github.com/gleam-lang/gleam/blob/main/stdlib/string/gleam
- テスト自体のコード：https://github.com/gleam-lang/gleam/blob/main/test/string.gleam