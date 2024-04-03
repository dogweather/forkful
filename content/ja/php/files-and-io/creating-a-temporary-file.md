---
date: 2024-01-20 17:41:07.485039-07:00
description: "How to (\u65B9\u6CD5): PHP\u306E `tmpfile()` \u95A2\u6570\u3092\u4F7F\
  \u7528\u3057\u3066\u7C21\u5358\u306B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\
  \u6210\u3057\u307E\u3059\u3002`fopen()` \u95A2\u6570\u3067 `php://temp` \u30B9\u30C8\
  \u30EA\u30FC\u30E0\u3092\u4F7F\u3046\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\
  \u3053\u3053\u306B\u4F8B\u3068\u30A2\u30A6\u30C8\u30D7\u30C3\u30C8\u3092\u793A\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.273769-06:00'
model: gpt-4-1106-preview
summary: "PHP\u306E `tmpfile()` \u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u7C21\u5358\
  \u306B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3057\u307E\u3059\u3002\
  `fopen()` \u95A2\u6570\u3067 `php://temp` \u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u4F7F\
  \u3046\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u306B\u4F8B\u3068\
  \u30A2\u30A6\u30C8\u30D7\u30C3\u30C8\u3092\u793A\u3057\u307E\u3059."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
