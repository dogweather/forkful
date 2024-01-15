---
title:                "テキストファイルの書き方"
html_title:           "PHP: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

テキストファイルを書く理由は様々ですが、主なものはデータの永続性です。データベースやプログラムではなく、簡単なテキストファイルを使用することで、より柔軟な方法でデータを保管することができます。

## How To

テキストファイルを書くには、まずPHPのファイル操作関数を使用します。以下のコードを使用して、新しいテキストファイルを作成し、データを書き込むことができます。

```PHP
<?php

// ファイルを作成し、書き込み用に開く
$myfile = fopen("textfile.txt", "w") or die("Unable to open file!");

// テキストファイルにデータを書き込む
$txt = "This is a sample text.";
fwrite($myfile, $txt);

// ファイルを閉じる
fclose($myfile);
```

上記のコードを実行すると、PHPファイルと同じディレクトリに "textfile.txt" という名前の新しいファイルが作成されます。テキストファイルを開くと、"This is a sample text." というテキストが書き込まれていることがわかります。

また、既存のテキストファイルを編集することもできます。以下のコードを使用して、既存のテキストファイルに新しいデータを追記することができます。

```PHP
<?php

// ファイルを開き、追記用に開く
$myfile = fopen("textfile.txt", "a") or die("Unable to open file!");

// 既存のデータの後に新しいデータを追記
$txt = "This is a new line.";
fwrite($myfile, $txt);

// ファイルを閉じる
fclose($myfile);
```

上記のコードを実行すると、既存のテキストファイルの最後に "This is a new line." というテキストが追記されることがわかります。

## Deep Dive

ファイル操作関数の詳細については、PHPの公式ドキュメントを参照してください。また、テキストファイルを書く際には、文字エンコーディングにも注意が必要です。データを書き込む前に、適切な文字エンコーディングを指定することで、日本語や他の言語のテキストも正しく書き込むことができます。

## See Also

- PHPのファイル操作関数: https://www.php.net/manual/ja/ref.filesystem.php
- 文字エンコーディングについて: https://www.php.net/manual/ja/function.mb-convert-encoding.php