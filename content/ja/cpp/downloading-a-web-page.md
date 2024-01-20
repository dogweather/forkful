---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページをダウンロードするとは、インターネット上のコンテンツをローカルのシステムに保存することを指します。プログラマーがこれを行う理由は多岐にわたりますが、主にデータ分析、コンテンツのバックアップ、またはオフラインでの使用が目的です。

## やり方:

以下にC++を用いたウェブページのダウンロード方法を示していきます。  
私たちは`cURLpp`というライブラリを使用しますので、まずはそのインストールから始めましょう。

```C++
sudo apt install libcurl4-openssl-dev libcurlpp-dev
```

次に以下のコードを用いてウェブページをダウンロードします。

```C++
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>

int main() {
    curlpp::Cleanup cleaner;
    curlpp::Easy request;

    request.setOpt(new curlpp::options::Url("http://example.com"));
    request.setOpt(new curlpp::options::WriteToStream(&std::cout));

    request.perform();
    return 0;
}
```

このコードを実行すれば、指定のURLのコンテンツがコンソールに表示されます。

## ディープダイブ

ウェブページをダウンロードするという概念は、インターネットが一般的に利用可能になった1990年代初頭から存在します。この手法が広く用いられるようになった背景には、情報の収集やウェブスクレイピングといったニーズがあります。

代替手段としては`wget`や`httrack`などのコマンドラインツール、またPythonの`requests`やNode.jsの`axios`などの他のプログラミング言語を使用することもあります。

先ほどのC++のコードでは`cURLpp`ライブラリを使用しました。これは有名なコマンドラインツールである`cURL`のC++ラッパーです。このライブラリにはHTTPリクエストを簡単に発行できる機能が含まれており、ここではGETリクエストを発行してウェブページのコンテンツを取得しています。

## 参照

- cURLpp公式ドキュメント: [こちら](https://www.cpp-netlib.org/)
- cURL公式ドキュメント: [こちら](https://curl.haxx.se/)
- Wgetマニュアル: [こちら](https://www.gnu.org/software/wget/manual/wget.html) 
- Python Requests: [こちら](https://requests.readthedocs.io/en/master/) 
- Node.js axios: [こちら](https://github.com/axios/axios)