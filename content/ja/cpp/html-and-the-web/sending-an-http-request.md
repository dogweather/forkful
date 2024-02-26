---
date: 2024-01-20 17:59:42.298395-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3069\
  \u3093\u306A\u3053\u3068\uFF1F \u305D\u308C\u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u304C\u5B9F\u884C\u3059\u308B\u7406\u7531\u306F\uFF1FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306F\u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u554F\u3044\
  \u5408\u308F\u305B\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u9001\u4FE1\u3057\u305F\
  \u308A\u3059\u308B\u305F\u3081\u306E\u4ED5\u7D44\u307F\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306FAPI\u306E\u5229\u7528\u3001\u30B5\u30FC\u30D0\u30FC\
  \u3068\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u3001Web\u30B5\u30A4\u30C8\u306E\u30B3\
  \u30F3\u30C6\u30F3\u30C4\u53D6\u5F97\u306A\u3069\u3067\u3053\u306E\u6280\u8853\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.506431-07:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3069\u3093\
  \u306A\u3053\u3068\uFF1F \u305D\u308C\u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u5B9F\u884C\u3059\u308B\u7406\u7531\u306F\uFF1FHTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u306F\u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u554F\u3044\u5408\
  \u308F\u305B\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u9001\u4FE1\u3057\u305F\u308A\
  \u3059\u308B\u305F\u3081\u306E\u4ED5\u7D44\u307F\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306FAPI\u306E\u5229\u7528\u3001\u30B5\u30FC\u30D0\u30FC\u3068\
  \u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u3001Web\u30B5\u30A4\u30C8\u306E\u30B3\u30F3\
  \u30C6\u30F3\u30C4\u53D6\u5F97\u306A\u3069\u3067\u3053\u306E\u6280\u8853\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るってどんなこと？ それをプログラマーが実行する理由は？HTTPリクエストは、Webサーバーに情報を問い合わせたり、データを送信したりするための仕組みです。プログラマーはAPIの利用、サーバーとのデータ交換、Webサイトのコンテンツ取得などでこの技術を使用します。

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
