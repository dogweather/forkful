---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)

テキストファイルの読み込みとは、文字データを持つファイル内容をPHPプログラムで取得する行為です。これは、データの利用や分析、さらにはウェブページの動的内容の生成などのために、プログラマが行います。

## How to: (方法:)

```PHP
<?php
$file = 'file.txt';
$contents = file_get_contents($file);
echo $contents;
?>
```

上記の例清ますと、'file.txt'というテキストファイルの内容が出力されます。もし、`$file`は存在しない場合、エラーメッセージが出力されます。

```
Hello world!
This is content of file.txt.
```

## Deep Dive (深掘り:)

テキストファイルの読み込みは古くからある機能で、PHPの初期バージョンから存在します。もともとは、ファイルデータの操作や利用を容易にするために導入されました。

代替方法として、`fopen`と`fread`を利用することも可能です。これらを利用すると、大きいファイルやバイナリファイルの操作が可能になります。ただし、より簡潔で直感的な`file_get_contents`の方が一般的なテキストファイル読み込みには適しています。

`file_get_contents`の内部実装には、ストリームラッパーとバッファリングが用いられています。これにより、効率的かつ安全なファイル読み込みが可能になっています。

## See Also (関連情報)

- 公式PHPマニュアルの`file_get_contents`のページ: [https://www.php.net/manual/en/function.file-get-contents.php](https://www.php.net/manual/en/function.file-get-contents.php)
- 'PHPのストリームラッパー'についての説明: [http://php.net/wrappers](http://php.net/wrappers)