---
title:                "Gleam: 文字列の大文字化"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字に変換することにエンゲージする必要があるのか、その理由を説明します。

## 方法
```Gleam
let string = "hello, world"
let capitalized = string.to_upper
hello, world
HELLO, WORLD
```

## 詳細を掘り下げる

文字列を大文字に変換することは、テキスト処理において非常に一般的な操作です。例えば、ユーザーの入力を正規化するために使われることがあります。また、文字列の比較を行う際にも大文字小文字の違いを無視する必要がある場合があります。大文字に変換することで、文字列の比較をより正確に行うことができます。

## See Also
- [Gleam 公式ドキュメント](https://gleam.run/)
- [Gleam チュートリアル](https://gleam.run/tutorial/)
- [Gleam コミュニティフォーラム](https://github.com/gleam-lang/gleam/issues)