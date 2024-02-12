---
title:                "ウェブページのダウンロード"
aliases:
- /ja/cpp/downloading-a-web-page/
date:                  2024-01-20T17:43:32.787190-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
Webページをダウンロードするとは、インターネットから特定のページの内容を取得することです。これをプログラマが行う理由は、データの収集、コンテンツの解析、または自動化タスクの一環としてです。

## How to / 方法
C++でウェブページをダウンロードするには、ライブラリを使います。ここでは`cpr`（C++ Requests, Python Requestsライブラリに着想を得たもの）を使用します。`cpr`はシンプルかつ現代的なAPIを提供します。

依存関係（`cpr`ライブラリ）をインストールします。

```bash
vcpkg install cpr
```

サンプルコード：

```C++
#include <cpr/cpr.h>
#include <iostream>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/html"});
    if (r.status_code == 200) { // HTTP 200 OK
        std::cout << r.text << std::endl;  // r.text contains the webpage content
    } else {
        std::cout << "Failed to retrieve web page." << std::endl;
    }
    return 0;
}
```

実行結果：

```
<!DOCTYPE html>
<html>
<body>
<h1>Herman Melville - Moby-Dick</h1>
...
</body>
</html>
```

エラーハンドリングは省略していますが、実際のプロジェクトでは必要です。

## Deep Dive / 掘り下げ
WebページダウンロードはHTTPプロトコルで行われます。過去には`libcurl`や`Boost.Beast`のような低レベルのライブラリが使われていました。だが、`cpr`のような新しいライブラリはもっと簡単にHTTPリクエストを扱えるようになっています。

`cpr`はバックエンドで`libcurl`を使用していますが、使用者には簡潔なインターフェースを提供します。このような抽象化により、プログラマはネットワーク通信の複雑さを意識することなく、コードを書くことができます。

別の手段を使うこともできます。例えば、標準C++ライブラリだけでHTTP通信を実装することも可能ですが、多くのボイラープレートコードを必要とし、エラー処理が複雑になります。適切なツールやライブラリを選ぶことで、効率的かつ安全に作業できます。

## See Also / 関連リンク
- cpr GitHub リポジトリ: [https://github.com/libcpr/cpr](https://github.com/libcpr/cpr)
- libcurl (cURL): [https://curl.haxx.se/libcurl/](https://curl.haxx.se/libcurl/)
- HTTP プロトコルについて: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- Boost.Beast ライブラリ: [https://www.boost.org/doc/libs/release/libs/beast/](https://www.boost.org/doc/libs/release/libs/beast/)
