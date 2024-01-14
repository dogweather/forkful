---
title:                "Fish Shell: 「標準エラーへの書き込み」"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングでは時々エラーメッセージを見かけることがあるでしょう。そのうちのいくつかは標準エラー出力に表示されます。標準エラー出力とは何なのか、そしてそれを使うことの利点は何なのか、この記事ではその理由を説明します。

## 使い方

Fish Shellでは、標準エラー出力にメッセージを表示する簡単な方法があります。以下のようなコードを使用することができます。

```Fish Shell
echo "エラーメッセージ" >&2
```

このコードを実行すると、"エラーメッセージ"が標準エラー出力に表示されます。また、`>&2`の部分は、メッセージを標準エラー出力にリダイレクトするためのものです。

さらに、複数のメッセージを表示する場合は、下記のようにも書くことができます。

```Fish Shell
{
    echo "エラーメッセージ1"
    echo "エラーメッセージ2"
} >&2
```

このように、複数のコマンドを`{}`の中にまとめ、`>&2`で標準エラー出力へとリダイレクトすることで、複数のエラーメッセージを一括して表示することができます。

また、変数を使ってエラーメッセージを表示することもできます。

```Fish Shell
set error "エラーメッセージ"
echo $error >&2
```

このように、変数に格納したメッセージを`>&2`で標準エラー出力にリダイレクトすることで、より柔軟にエラーメッセージを表示することができます。

## 深堀り

標準エラー出力は、通常の標準出力とは別のストリームです。つまり、通常の標準出力にリダイレクトされたメッセージと、標準エラー出力にリダイレクトされたメッセージは別々の場所に表示されます。

エラーメッセージを標準エラー出力に表示することで、コマンドの実行中に発生したエラーをすぐに確認することができます。また、エラーメッセージを標準出力に表示するよりも、より詳細な情報を表示することができます。

## はじめてはじめる

標準エラー出力についてのさらなる情報を知りたい方は、以下のリンクを参考にしてください。

[Fish Shell標準エラー出力について](https://fishshell.com/docs/current/tutorial.html#escape-completed-commands)

[標準エラー出力に関するFAQ](https://fishshell.com/docs/current/faq.html#stderr-to-stdout)

[標準エラー出力のディレクトリを変更する方法](https://fishshell.com/docs/current/faq.html#use-stderr)

[後述のように、標準エラー出力をリダイレクトする方法についてのより詳細な情報](https://fishshell.com/docs/current/tutorial.html#redirection)

## 関連情報

[関連記事](https://www.example.com)

[関連ブログ](https://www.example.com/blog)

[Fish Shellドキュメント](https://fishshell.com/docs/current)