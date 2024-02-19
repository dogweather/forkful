---
aliases:
- /ja/fish-shell/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:41.094706-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\
  \u4ED8\u3051\u3066\u9001\u308B\u3063\u3066\uFF1F\u305D\u308C\u306F\u3001\u30E6\u30FC\
  \u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30A6\
  \u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u306B\u30ED\u30B0\u30A4\u30F3\u3059\u308B\u3053\
  \u3068\u3002\u3069\u3046\u3057\u3066\u3084\u308B\u306E\uFF1F\u5B89\u5168\u306B\u30C7\
  \u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30AA\u30F3\u30E9\u30A4\u30F3\
  \u30B5\u30FC\u30D3\u30B9\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u3060\u3088\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.309262
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u4ED8\
  \u3051\u3066\u9001\u308B\u3063\u3066\uFF1F\u305D\u308C\u306F\u3001\u30E6\u30FC\u30B6\
  \u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30A6\u30A7\
  \u30D6\u30B5\u30FC\u30D3\u30B9\u306B\u30ED\u30B0\u30A4\u30F3\u3059\u308B\u3053\u3068\
  \u3002\u3069\u3046\u3057\u3066\u3084\u308B\u306E\uFF1F\u5B89\u5168\u306B\u30C7\u30FC\
  \u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30AA\u30F3\u30E9\u30A4\u30F3\u30B5\
  \u30FC\u30D3\u30B9\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u3060\u3088\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストに基本認証を付けて送るって？それは、ユーザー名とパスワードを使ってウェブサービスにログインすること。どうしてやるの？安全にデータを取得したり、オンラインサービスを操作するためだよ。

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
