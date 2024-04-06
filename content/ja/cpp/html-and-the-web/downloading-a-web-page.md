---
date: 2024-01-20 17:43:32.787190-07:00
description: "How to / \u65B9\u6CD5 C++\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\
  \u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u306B\u306F\u3001\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F`cpr`\uFF08\
  C++ Requests, Python Requests\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u7740\u60F3\u3092\
  \u5F97\u305F\u3082\u306E\uFF09\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002`cpr`\u306F\
  \u30B7\u30F3\u30D7\u30EB\u304B\u3064\u73FE\u4EE3\u7684\u306AAPI\u3092\u63D0\u4F9B\
  \u3057\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.058500-06:00'
model: gpt-4-1106-preview
summary: "How to / \u65B9\u6CD5 C++\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\
  \u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u306B\u306F\u3001\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F`cpr`\uFF08\
  C++ Requests, Python Requests\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u7740\u60F3\u3092\
  \u5F97\u305F\u3082\u306E\uFF09\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002`cpr`\u306F\
  \u30B7\u30F3\u30D7\u30EB\u304B\u3064\u73FE\u4EE3\u7684\u306AAPI\u3092\u63D0\u4F9B\
  \u3057\u307E\u3059\u3002 \u4F9D\u5B58\u95A2\u4FC2\uFF08`cpr`\u30E9\u30A4\u30D6\u30E9\
  \u30EA\uFF09\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

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
