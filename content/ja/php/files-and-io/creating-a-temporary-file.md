---
date: 2024-01-20 17:41:07.485039-07:00
description: "PHP\u3067\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\
  \u308B\u3068\u306F\u3001\u77ED\u671F\u9593\u306B\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\
  \u3059\u308B\u305F\u3081\u306E\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\
  \u4E00\u6642\u7684\u306A\u51E6\u7406\u3084\u3001\u5927\u91CF\u30C7\u30FC\u30BF\u306E\
  \u65AD\u7247\u7684\u306A\u8AAD\u307F\u66F8\u304D\u3001\u307E\u305F\u306F\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u304C\u5FC5\u8981\u306A\u6642\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.410044
model: gpt-4-1106-preview
summary: "PHP\u3067\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\u308B\
  \u3068\u306F\u3001\u77ED\u671F\u9593\u306B\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\
  \u308B\u305F\u3081\u306E\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u4E00\
  \u6642\u7684\u306A\u51E6\u7406\u3084\u3001\u5927\u91CF\u30C7\u30FC\u30BF\u306E\u65AD\
  \u7247\u7684\u306A\u8AAD\u307F\u66F8\u304D\u3001\u307E\u305F\u306F\u30BB\u30AD\u30E5\
  \u30EA\u30C6\u30A3\u304C\u5FC5\u8981\u306A\u6642\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
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
