---
date: 2024-01-20 17:43:32.787190-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u7279\
  \u5B9A\u306E\u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u3053\u308C\u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\u884C\
  \u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u306E\u53CE\u96C6\u3001\u30B3\u30F3\
  \u30C6\u30F3\u30C4\u306E\u89E3\u6790\u3001\u307E\u305F\u306F\u81EA\u52D5\u5316\u30BF\
  \u30B9\u30AF\u306E\u4E00\u74B0\u3068\u3057\u3066\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.664181
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u7279\u5B9A\
  \u306E\u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u3053\u308C\u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\u884C\u3046\
  \u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u306E\u53CE\u96C6\u3001\u30B3\u30F3\u30C6\
  \u30F3\u30C4\u306E\u89E3\u6790\u3001\u307E\u305F\u306F\u81EA\u52D5\u5316\u30BF\u30B9\
  \u30AF\u306E\u4E00\u74B0\u3068\u3057\u3066\u3067\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
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
