---
date: 2024-01-20 18:01:41.094706-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.601762-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) HTTP\u306E\u57FA\u672C\u8A8D\u8A3C\u306F\u3001RFC7617\u3067\
  \u5B9A\u7FA9\u3055\u308C\u3066\u3044\u308B\u53E4\u3044\u65B9\u6CD5\u3060\u3051\u3069\
  \u3001\u7C21\u5358\u306B\u5C0E\u5165\u3067\u304D\u308B\u304B\u3089\u4ECA\u3082\u4F7F\
  \u308F\u308C\u3066\u3044\u308B\u3002\u305F\u3060\u3057\u3001HTTPS\u3092\u4F7F\u308F\
  \u306A\u3044\u3068\u3001\u30D1\u30B9\u30EF\u30FC\u30C9\u304C\u6697\u53F7\u5316\u3055\
  \u308C\u305A\u306B\u9001\u4FE1\u3055\u308C\u308B\u30EA\u30B9\u30AF\u304C\u3042\u308B\
  \u3002\u3088\u308A\u5B89\u5168\u306A\u4EE3\u66FF\u65B9\u6CD5\u3068\u3057\u3066OAuth\u304C\
  \u3042\u308B\u3002\u5B9F\u88C5\u3059\u308B\u6642\u306F\u3001\u30D1\u30B9\u30EF\u30FC\
  \u30C9\u306A\u3069\u306E\u91CD\u8981\u306A\u60C5\u5831\u3092\u30B3\u30DE\u30F3\u30C9\
  \u30E9\u30A4\u30F3\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u30CF\u30FC\u30C9\u30B3\
  \u30FC\u30C7\u30A3\u30F3\u30B0\u3059\u308B\u306E\u3067\u306F\u306A\u304F\u3001\u74B0\
  \u5883\u5909\u6570\u3092\u901A\u3058\u3066\u5B89\u5168\u306B\u6E21\u3059\u65B9\u6CD5\
  \u3092\u691C\u8A0E\u3057\u3088\u3046\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## How to: (方法)
```Fish Shell
# ユーザー名とパスワードを64ビットエンコード用の変数にセット
set -l credentials (echo -n "username:password" | base64)

# エンコードした資格情報を使ってHTTPリクエストを送る
curl -H "Authorization: Basic $credentials" https://example.com/api/data
```

サンプル出力:
```
{
    "data": "ここに取得したデータが入る"
}
```

## Deep Dive (深掘り)
HTTPの基本認証は、RFC7617で定義されている古い方法だけど、簡単に導入できるから今も使われている。ただし、HTTPSを使わないと、パスワードが暗号化されずに送信されるリスクがある。より安全な代替方法としてOAuthがある。実装する時は、パスワードなどの重要な情報をコマンドラインやスクリプトにハードコーディングするのではなく、環境変数を通じて安全に渡す方法を検討しよう。

## See Also (関連情報)
- [RFC7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [cURL Manual](https://curl.se/docs/manual.html)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [HTTP authentication on MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
