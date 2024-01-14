---
title:                "Gleam: 「文字列を小文字に変換する」"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
人々はなぜ文字列を小文字に変換することを行うのか、その理由はなんでしょうか。一言で説明すると、小文字に変換することで文字列をより柔軟に扱うことができるようになります。

## 方法
以下のようなGleamのコードを使って、文字列を小文字に変換する方法を示します。

```Gleam
let string = "Hello World"
let lower_string = String.to_lower_case(string)
```

上記のコードを実行すると、`lower_string`には"hello world"という文字列が格納されます。これにより、大文字や小文字の違いを気にしなくても文字列を比較することができるようになります。

## ディープダイブ
文字列を小文字に変換する方法をより詳しく説明します。まず、Gleamの標準ライブラリに含まれる`String`モジュールには、`to_lower_case`以外にも文字列を操作するための便利な関数があります。例えば、`contains`関数を使うと任意の文字列が他の文字列に含まれているかどうかを判定することができます。また、`starts_with`や`ends_with`といった関数を使うことで、文字列が特定の文字列で始まるか終わるかを簡単にチェックすることができます。

さらに、日本語のようにUnicode文字がたくさん含まれる文字列でも、`to_lower_case`関数を使うことで正しく小文字に変換することができます。これはGleamがUnicodeをサポートしているためです。

## それでは次の記事をどうぞ
これらの記事も合わせてお読みください。

- [Gleamの公式ドキュメント](https://gleam.run/)
- [Gleamで文字列を操作する方法](https://gleam.run/articles/strings.html)
- [Gleamで文字列をフォーマットする方法](https://gleam.run/articles/printf.html)

## 参考リンク
- [GleamのGitHubリポジトリ](https://github.com/gleam-lang/gleam)
- [Unicodeについての詳細な説明](https://unicode.org/)