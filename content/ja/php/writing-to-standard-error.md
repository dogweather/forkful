---
title:                "標準エラーへの書き込み"
html_title:           "PHP: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
プログラミング言語としてよく使われるPHPでは、エラーが発生した際にエラーメッセージを標準エラー出力に表示することができます。この標準エラー出力を活用することで、開発作業をよりスムーズに行うことができるようになります。 

## やり方
PHPで標準エラーにメッセージを出力するには、```error_log()```関数を使用します。以下のように記述することで、エラーメッセージを標準エラー出力に表示することができます。

```PHP
error_log("エラーが発生しました。", STDERR);
```

上記の例では、```STDERR```にメッセージが出力されるようになっています。このようにすることで、標準的なエラーメッセージよりも詳細な情報を出力することができます。

## ディープダイブ
標準エラー出力を使用することで、開発中のコードのバグをより早く発見することができます。また、リダイレクトを使用せずにエラーメッセージを管理することができるため、より柔軟にプログラミングを行うことができます。さらに、```error_log()```関数を使用することで、特定の場合にのみエラーメッセージを出力するように制御することもできます。

## 関連記事
- [PHPの公式ドキュメント：error_log()関数](https://www.php.net/manual/ja/function.error-log.php)
- [PHPエラーハンドリングについての記事](https://career-goals.co.jp/blog/700/)