---
date: 2024-01-20 18:01:41.094706-07:00
description: "How to: (\u65B9\u6CD5) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.519103-06:00'
model: gpt-4-1106-preview
summary: ''
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
