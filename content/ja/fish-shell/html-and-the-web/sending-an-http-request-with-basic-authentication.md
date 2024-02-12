---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases:
- ja/fish-shell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:41.094706-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request-with-basic-authentication.md"
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
