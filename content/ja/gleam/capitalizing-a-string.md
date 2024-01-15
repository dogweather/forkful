---
title:                "文字列を大文字化する"
html_title:           "Gleam: 文字列を大文字化する"
simple_title:         "文字列を大文字化する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換することの利点は何でしょうか？それを理解するためには、Gleamの標準ライブラリで提供されるString.capitalize関数をご紹介します。

## 使い方

Gleamでは、String.capitalize関数を使って文字列を大文字に変換することができます。下記のコード例をご覧ください。

```

Gleam

let name = "gleam"
name
|> String.capitalize // "Gleam"
```

上記のように、文字列を変数に定義し、それに対してString.capitalize関数を適用することで、最初の文字を大文字に変換することができます。

また、他の言語と同様に、文字列を直接引数として渡すこともできます。下記のコード例をご覧ください。

```

Gleam

String.capitalize("gleam") // "Gleam"
```

さらに、文字列の中に大文字が含まれている場合は、そのまま返されることに注意してください。大文字に変換するのは最初の文字だけです。下記のコード例をご覧ください。

```

Gleam

let name = "GLEAM"
name // "GLEAM"
|> String.capitalize // "GLEAM"
```

## ディープダイブ

String.capitalize関数は、文字列を大文字に変換する際の内部処理を深く理解するのに役立ちます。例えば、文字列を最初の文字だけ大文字に変換するということは、文字列をリストに変換し、最初の文字を大文字にし、再び文字列に変換するという処理になります。

また、Gleamには他にも便利な文字列操作関数が提供されています。例えば、String.slice関数を使うと文字列の一部分を抜き出すことができます。

## 他にもチェックしてみてください

[公式ドキュメント](https://gleam.run/)からGleamについてもっと詳しく学ぶことができます。

See Also:
見てみてね
- [GleamのGitHubリポジトリ](https://github.com/gleam-lang/gleam) - 最新情報やソースコードをチェックしよう
- [GleamのSlackチャンネル](https://join.slack.com/t/gleam-lang/shared_invite/zt-gkw45z6o-vmB50FOITDoUn9P_ff~JzQ) - コミュニティに参加してみんなと交流しよう
- [GleamのTwitterアカウント](https://twitter.com/gleamlang) - ツイートをフォローして新しいアップデートをゲットしよう