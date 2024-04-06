---
date: 2024-01-20 17:59:42.298395-07:00
description: "How to: (\u3084\u308A\u65B9) C++\u3067\u306F\u3001\u3044\u304F\u3064\
  \u304B\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4E00\u4F8B\u3068\
  \u3057\u3066`cpr`\u3068\u3044\u3046\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\
  \u3066GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u30B3\u30FC\u30C9\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.359397-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) C++\u3067\u306F\u3001\u3044\u304F\u3064\u304B\u306E\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u6271\
  \u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4E00\u4F8B\u3068\u3057\u3066\
  `cpr`\u3068\u3044\u3046\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\u3066GET\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\
  \u307E\u3057\u3087\u3046\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## How to: (やり方)
C++では、いくつかのライブラリでHTTPリクエストを扱うことができます。一例として`cpr`というライブラリを使ってGETリクエストを送るコードを見てみましょう。

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    // httpbin.orgで簡易的なGETリクエストを送る
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    
    // 結果を表示
    std::cout << "Status Code: " << r.status_code << std::endl;  // HTTPステータスコード
    std::cout << "Response Body: " << std::endl << r.text;       // レスポンスボディ
}
```

実行結果はこんな感じです:

```
Status Code: 200
Response Body: {
  ...
  // ここにJSON形式でのレスポンスが続きます
}
```

`cpr`ライブラリを使えば、簡単にHTTPリクエストを送るプログラムを書けるわけです！

## Deep Dive (深掘り)
HTTPリクエストの送信は、初期のインターネットから実行されています。HTTP/1.1やHTTP/2のように、時代と共にプロトコルも進化し、より効率的な通信が可能になっています。

C++でHTTPリクエストを送るには、事前に`libcurl`のような外部ライブラリをインストールする方法がありましたが、最近では`Boost.Beast`、`cpr`のように、使いやすさを追求したライブラリが普及しています。

また、実装の詳細については、リクエストのタイプ（GET, POST, DELETEなど）、ヘッダー情報やタイムアウト、エラーハンドリングなど、注意すべき点が数多くあります。これらの知識は、安定した通信のためには欠かせません。

## See Also (関連情報)
- `cpr` GitHubリポジトリ: https://github.com/libcpr/cpr
- HTTPプロトコル詳細 (MDN Web Docs): https://developer.mozilla.org/en-US/docs/Web/HTTP
- `libcurl`公式サイト: https://curl.haxx.se/libcurl/
- Boost.Beastドキュメント: https://www.boost.org/doc/libs/release/libs/beast/ 

プログラミングに関する冒険は、新しい知識を一つずつ積み上げていくことから始まります。適切なライブラリを選んで、ネットワークを介したやり取りをマスターしましょう！
