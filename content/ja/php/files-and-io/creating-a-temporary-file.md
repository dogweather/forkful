---
title:                "一時ファイルの作成"
aliases: - /ja/php/creating-a-temporary-file.md
date:                  2024-01-20T17:41:07.485039-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
PHPで一時ファイルを作成するとは、短期間にデータを保存するためのファイルを作ることです。プログラマーはデータの一時的な処理や、大量データの断片的な読み書き、またはセキュリティが必要な時にこれを行います。

## How to (方法):
PHPの `tmpfile()` 関数を使用して簡単に一時ファイルを作成します。`fopen()` 関数で `php://temp` ストリームを使うこともできます。ここに例とアウトプットを示します。

```php
<?php
// tmpfile() を使用して一時ファイルを作成
$tempFile = tmpfile();

// 一時ファイルにデータを書き込む
fwrite($tempFile, "Temporary data here.");

// 一時ファイルへのポインタをリセット
rewind($tempFile);

// 保存されたデータを読み込む
echo fread($tempFile, 1024);

// 一時ファイルを閉じる（自動的に削除される）
fclose($tempFile);

// fopen() と php://temp を使用して一時ファイルを作成
$tempStream = fopen('php://temp', 'r+');

// 一時ストリームにデータを書き込む
fwrite($tempStream, "Temporary stream data here.");

// ストリームへのポインタをリセット
rewind($tempStream);

// 保存されたデータを読み込む
echo fread($tempStream, 1024);

// ストリームを閉じる
fclose($tempStream);
?>
```

Sample Output:
```
Temporary data here.
Temporary stream data here.
```

## Deep Dive (掘り下げ):
一時ファイルの使い方は昔からあり、データの一時的な保管や安全な転送方法として機能しています。PHPの `tmpfile()` 関数はシステムの一時ディレクトリにファイルを作成し、ファイルハンドルを返します。一時ファイルは `fclose()` 関数を呼び出すと自動的に削除されるため、クリーンアップが簡単です。`php://temp` ストリームを使用すると、データが小さい場合はメモリに保存され、大きい場合はファイルシステムに落ちます。これは性能およびリソースの利用により優れた柔軟性を提供します。

## See Also (関連情報):
- [Filesystem Functions in PHP Official Documentation](https://www.php.net/manual/en/ref.filesystem.php)
- [PHP `tmpfile()` Function](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP I/O Streams](https://www.php.net/manual/en/wrappers.php.php)
