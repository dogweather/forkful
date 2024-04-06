---
date: 2024-01-20 17:59:27.766581-07:00
description: "How to: (\u65B9\u6CD5) Fish Shell\u3067\u306F`curl`\u30B3\u30DE\u30F3\
  \u30C9\u3092\u4F7F\u7528\u3057\u3066HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u7C21\
  \u5358\u306B\u9001\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306F\u57FA\u672C\u7684\u306AGET\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u4F8B\u3067\
  \u3059\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.515904-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Fish Shell\u3067\u306F`curl`\u30B3\u30DE\u30F3\u30C9\u3092\
  \u4F7F\u7528\u3057\u3066HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u7C21\u5358\u306B\
  \u9001\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u57FA\
  \u672C\u7684\u306AGET\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u4F8B\u3067\u3059\uFF1A\
  ."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## How to: (方法)
Fish Shellでは`curl`コマンドを使用してHTTPリクエストを簡単に送ることができます。以下は基本的なGETリクエストの例です：

```Fish Shell
curl http://example.com/api
```

レスポンスの例：

```
{"status":"success", "data": {"id":1, "name":"Foo"}}
```

POSTリクエストでJSONデータを送信するには：

```Fish Shell
curl -X POST -H "Content-Type: application/json" -d '{"key1":"value1", "key2":"value2"}' http://example.com/api
```

サーバーからの応答例：

```
{"status":"success", "message":"Data received"}
```

## Deep Dive (詳細情報)
HTTPリクエストの仕組みは90年代初頭からあり、インターネットとともに進化してきました。Pythonには`requests`ライブラリ、JavaScriptには`axios`や`fetch`といった代替手段があります。Fish Shellでの実装では、多くの場合`curl`が使われますが、より詳細な制御が必要な場合は`wget`や`httpie`などの他のツールを選択することもできます。

## See Also (関連情報)
- [curl公式ドキュメント](https://curl.se/docs/manual.html)
- [HTTPie公式サイト](https://httpie.io/)
- [Fish Shellドキュメント](https://fishshell.com/docs/current/index.html)
- [MDN Web Docs: HTTPリクエストメソッド](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)
