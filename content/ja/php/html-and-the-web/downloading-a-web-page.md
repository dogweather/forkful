---
date: 2024-01-20 17:44:29.285189-07:00
description: "How to \u6700\u521D\u306B `curl` \u306F1980\u5E74\u4EE3\u5F8C\u534A\u306B\
  \u958B\u767A\u3055\u308C\u305F\u3002PHP\u3067\u306F `cURL` \u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u3001\u3088\u308A\u7C21\u5358\u306B\u30A6\u30A7\u30D6\
  \u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3067\u304D\u307E\u3059\
  \u3002`file_get_contents()` \u306E\u3088\u3046\u306A\u4ED6\u306E\u95A2\u6570\u3082\
  \u3042\u308A\u307E\u3059\u304C\u3001`cURL`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.160012-06:00'
model: gpt-4-1106-preview
summary: "How to \u6700\u521D\u306B `curl` \u306F1980\u5E74\u4EE3\u5F8C\u534A\u306B\
  \u958B\u767A\u3055\u308C\u305F\u3002PHP\u3067\u306F `cURL` \u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u3001\u3088\u308A\u7C21\u5358\u306B\u30A6\u30A7\u30D6\
  \u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3067\u304D\u307E\u3059\
  \u3002`file_get_contents()` \u306E\u3088\u3046\u306A\u4ED6\u306E\u95A2\u6570\u3082\
  \u3042\u308A\u307E\u3059\u304C\u3001`cURL` \u306FHTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u306E\u30AA\u30D7\u30B7\u30E7\u30F3\u304C\u591A\u304F\u3001\u3088\u308A\u8907\u96D1\
  \u306A\u30BF\u30B9\u30AF\u306B\u5411\u3044\u3066\u3044\u307E\u3059\u3002`CURLOPT_RETURNTRANSFER`\
  \ \u3092 `true` \u306B\u8A2D\u5B9A\u3059\u308B\u3053\u3068\u3067\u3001\u7D50\u679C\
  \u3092\u76F4\u63A5\u30D6\u30E9\u30A6\u30B6\u306B\u51FA\u529B\u3059\u308B\u4EE3\u308F\
  \u308A\u306B\u5909\u6570\u306B\u4FDD\u5B58\u3057\u307E\u3059\u3002\u3053\u306E\u65B9\
  \u6CD5\u306FAPI\u304B\u3089\u30C7\u30FC\u30BF\u3092\u5F15\u3063\u5F35\u308B\u306E\
  \u306B\u3082\u3088\u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002 \u307E\u305F\u3001\
  \u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u306B\u306F\u6CD5\u7684\
  \u306A\u554F\u984C\u304C\u4F34\u3046\u5834\u5408\u304C\u3042\u308B\u305F\u3081\u3001\
  \u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u969B\u306F\u30A6\u30A7\u30D6\u30B5\
  \u30A4\u30C8\u306E\u5229\u7528\u898F\u7D04\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\
  \u304C\u5927\u5207\u3067\u3059\u3002"
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
