---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを基本認証で送信するとは、ユーザー名とパスワードをヘッダーに含めてサーバーにリクエスト送信を行うことです。これにより、プログラマは認証が必要なWebサービスに安全にアクセスできます。

## どうやって：

C++でHTTPリクエストを基本認証で送信する一例を示します。この例では、`CPR`というライブラリを使用します：

```C++
#include <cpr/cpr.h>

int main() {
    cpr::Authentication auth{"user", "pass"};

    cpr::Response r = cpr::Get(cpr::Url{"http://example.com"}, auth);

    std::cout << r.status_code << std::endl;
    std::cout << r.header["content-type"] << std::endl;
    std::cout << r.text << std::endl;

    return 0;
}
```

この例では、ユーザーネームとパスワードを含むAuthenticationオブジェクトを作成し、それをGETリクエストに添付しています。レスポンスのステータスコード、ヘッダー、本文は標準出力に出力されます。

## ディープダイブ

HTTPリクエストに基本認証を追加するプラクティスは、インターネットの初期、特に状態のないHTTPプロトコルの世界でよく見られました。しかし、現代のWeb開発では、セッションベースの認証やトークンベースの認証（OAuthなど）がより頻繁に利用されています。ただし、基本認証はそのシンプルさから一部の場合で有効な手段として残っています。

上記のコード例ではCPRライブラリを使用しましたが、他の有名なHTTPクライアントライブラリ、例えば `libcurl` や `Boost.Asio` なども同様のことが可能です。これらライブラリの選択は、そのAPIの簡潔さ、パフォーマンス、そして依存関係によって異なり、あなたのプロジェクトの要件によって選択するべきです。

## 参考情報

- CPRライブラリのドキュメンテーション: [https://docs.libcpr.org](https://docs.libcpr.org/)
- libcurlライブラリのドキュメンテーション: [https://curl.se/libcurl/c](https://curl.se/libcurl/c)
- Boost.Asioライブラリのドキュメンテーション: [https://www.boost.org/doc/libs/1_77_0/doc/html/boost_asio.html](https://www.boost.org/doc/libs/1_77_0/doc/html/boost_asio.html)