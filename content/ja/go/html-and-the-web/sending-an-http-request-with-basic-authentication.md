---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:39.540859-07:00
description: "\u65B9\u6CD5\uFF1A Go\u3067\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046\
  HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u884C\u3046\u306B\u306F\u3001`Authorization`\u30D5\
  \u30A3\u30FC\u30EB\u30C9\u3092\u542B\u3093\u3060\u30EA\u30AF\u30A8\u30B9\u30C8\u30D8\
  \u30C3\u30C0\u30FC\u3092\u9069\u5207\u306A\u5F62\u5F0F\u3067\u4F5C\u6210\u3057\u3001\
  \u8A8D\u8A3C\u60C5\u5831\u3092\u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u306F\u3001\u57FA\u672C\u8A8D\u8A3C\u304C\
  \u5FC5\u8981\u306AAPI\u30A8\u30F3\u30C9\u30DD\u30A4\u30F3\u30C8\u3078\u306EGET\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u5B9F\u884C\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\
  \u3066\u3044\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.715215-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Go\u3067\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046HTTP\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u884C\u3046\u306B\u306F\u3001`Authorization`\u30D5\
  \u30A3\u30FC\u30EB\u30C9\u3092\u542B\u3093\u3060\u30EA\u30AF\u30A8\u30B9\u30C8\u30D8\
  \u30C3\u30C0\u30FC\u3092\u9069\u5207\u306A\u5F62\u5F0F\u3067\u4F5C\u6210\u3057\u3001\
  \u8A8D\u8A3C\u60C5\u5831\u3092\u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u306F\u3001\u57FA\u672C\u8A8D\u8A3C\u304C\
  \u5FC5\u8981\u306AAPI\u30A8\u30F3\u30C9\u30DD\u30A4\u30F3\u30C8\u3078\u306EGET\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u5B9F\u884C\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\
  \u3066\u3044\u307E\u3059\uFF1A."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306E\u9001\u4FE1"
weight: 45
---

## 方法：
Goで基本認証を伴うHTTPリクエストを行うには、`Authorization`フィールドを含んだリクエストヘッダーを適切な形式で作成し、認証情報を追加する必要があります。以下の例は、基本認証が必要なAPIエンドポイントへのGETリクエストを実行する方法を示しています：

```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/api/data", nil)
	if err != nil {
		panic(err)
	}

	username := "yourUsername"
	password := "yourPassword"
    // クレデンシャルをエンコード
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // Authorizationヘッダーを設定
	req.Header.Add("Authorization", "Basic " + auth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("レスポンスステータス:", resp.Status)
}
```

このコードを実行すると、必要なAuthorizationヘッダーを付けて指定されたURLにGETリクエストが送信されます。出力はエンドポイントとサービスによって異なりますが、以下のようになるでしょう：

```
レスポンスステータス: 200 OK
```

## 詳細解説
HTTPリクエストの基本認証は、Webリソースへのアクセス制御を強制するための広くサポートされている方法です。それは単純に各リクエストにユーザー名とパスワードを送信するため、実装が簡単ですが、利用可能な最も安全な方法ではありません。主な欠点の一つは、SSL/TLSと一緒に使用しない限り、認証情報は（Base64は簡単にデコードされるため）クリアテキストで送信されることです。これにより、中間者攻撃によって機密情報が露出する可能性があります。

Goでこれらのリクエストを送信するには、直接`Authorization`ヘッダーを操作します。Goの標準ライブラリ（`net/http`）はHTTP(s)通信を扱うための強力な原始機能を提供していますが、比較的低レベルであり、開発者がHTTPリクエスト/レスポンスの扱いのさまざまな側面を手動で管理する必要があります。これによりプログラマーには多くの柔軟性が与えられますが、セキュリティの含意、エンコーディング、そして正しいヘッダーの管理についてより注意を払う必要があります。

より高いセキュリティを要求するアプリケーションの場合、OAuth2やJWT（JSON Web Tokens）などのより高度な認証システムを検討すべきです。これらのアプローチはより強固なセキュリティ機能を提供し、現代のAPIやサービスで広くサポートされています。Goの拡大するエコシステムには、これらのより安全な認証方法を実装するために開発者を支援する多数のライブラリやツール（`golang.org/x/oauth2`など）が含まれており、アプリケーションにおける安全で効果的で現代的な認証メカニズムを実装することが容易になります。
