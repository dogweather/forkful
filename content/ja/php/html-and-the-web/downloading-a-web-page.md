---
date: 2024-01-20 17:44:29.285189-07:00
description: "How to \u30B5\u30F3\u30D7\u30EB\u51FA\u529B (\u5B9F\u969B\u306EHTML\u306F\
  \u30B5\u30A4\u30C8\u306B\u3088\u308B)."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.774629-06:00'
model: gpt-4-1106-preview
summary: "How to \u30B5\u30F3\u30D7\u30EB\u51FA\u529B (\u5B9F\u969B\u306EHTML\u306F\
  \u30B5\u30A4\u30C8\u306B\u3088\u308B)."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to
```php
<?php
$url = "https://example.com"; // ダウンロードしたいウェブページのURL
$ch = curl_init($url); // cURLセッションを初期化

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // 結果を文字列として返す
$output = curl_exec($ch); // ウェブページの内容を$outputに保存

if ($output === false) {
    echo 'Error: ' . curl_error($ch);
} else {
    echo htmlspecialchars($output); // ウェブページの内容を安全に表示
}

curl_close($ch); // cURLセッションを閉じる
?>
```

サンプル出力 (実際のHTMLはサイトによる):
```
<!DOCTYPE html>
<html>
<head>
    <title>Example Domain</title>
</head>
<body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents.</p>
</body>
</html>
```

## Deep Dive
最初に `curl` は1980年代後半に開発された。PHPでは `cURL` ライブラリを使用し、より簡単にウェブページをダウンロードできます。`file_get_contents()` のような他の関数もありますが、`cURL` はHTTPリクエストのオプションが多く、より複雑なタスクに向いています。`CURLOPT_RETURNTRANSFER` を `true` に設定することで、結果を直接ブラウザに出力する代わりに変数に保存します。この方法はAPIからデータを引っ張るのにもよく使用されます。

また、ウェブスクレイピングには法的な問題が伴う場合があるため、ダウンロードする際はウェブサイトの利用規約を確認することが大切です。

## See Also
- PHP Manual on cURL: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)
- `file_get_contents()` documentation: [https://www.php.net/manual/en/function.file-get-contents.php](https://www.php.net/manual/en/function.file-get-contents.php)
- Web scraping legality: [https://en.wikipedia.org/wiki/Web_scraping#Legal_issues](https://en.wikipedia.org/wiki/Web_scraping#Legal_issues)
