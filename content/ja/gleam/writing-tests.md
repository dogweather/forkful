---
title:    "Gleam: テストの書き方"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ
テストを書く理由は何ですか。
テストを書くことで、コードの品質を向上させ、バグを早期に発見することができます。

## 方法
テストを書く方法は簡単です。Gleamのテストマクロを使用することで、簡単にテストコードを作成することができます。

```Gleam
test "Addition test" {
  expect(add(2, 3))
  |> to_equal(5)
}

test "Division test" {
  expect(divide(10, 2))
  |> to_equal(5)
}

test "String concatenation test" {
  expect(concat("Hello", "world"))
  |> to_equal("Hello world")
}
```

`expect`マクロを使用して、テストする関数の出力を定義し、`to_equal`関数を使用して期待する値を指定します。上記の例では、3つの異なるテストケースを示しています。

## 詳細
テストを書く際には、以下のことに注意する必要があります。

- テスト対象の関数が返す値の型を確認すること
- テストケースのバリエーションを考慮すること
- テストの可読性を高めるために、適切なテストケース名を使用すること
- 余分なコードを書かず、効率的なテストを行うこと

これらのポイントを抑えて、より高い品質のテストコードを作成することができます。

## 参考リンク
- [Gleam 公式ドキュメント](https://gleam.run/documentation)
- [Gleam テストの書き方](https://gleam-lang.org/news/introducing-tests)
- [Gleam のテストマクロの紹介](https://gleam-lang.org/news/introducing-tests)