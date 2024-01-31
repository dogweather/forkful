---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T17:59:42.298395-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request.md"
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
