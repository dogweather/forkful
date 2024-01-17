---
title:                "「Webページのダウンロード」"
html_title:           "Elixir: 「Webページのダウンロード」"
simple_title:         "「Webページのダウンロード」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 今回のお題：Webページのダウンロード

## 何？
Webページをダウンロードするというのは、単純に言えばインターネット上で公開されているページを自分のコンピューターに保存することです。プログラマーがこれを行う理由は、ある特定のページを自分のプログラムから呼び出すために使用するためです。

## 方法：
まずは、 ```HTTPoison``` ライブラリをダウンロードして利用します。次に、 ```HTTPoison.get``` という関数を使って、WebページのURLを指定します。その結果として、ページのコンテンツがバイナリ形式で返されます。以下は、ダウンロードしたページのコンテンツを表示するサンプルコードです。

```Elixir
require HTTPoison

# ページのURLを指定する
url = "https://example.com/"
# get関数を使ってページをダウンロードする
{:ok, response} = HTTPoison.get(url)

# ページのコンテンツを表示する
IO.puts(response.body)
```

出力結果は以下のようになります。

```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>

<div>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this
  domain in literature without prior coordination or asking for permission.</p>
</div>

</body>
</html>
```

## 詳細を探る：
Webページのダウンロードは、大昔から行われてきたプログラミングの基本的な機能です。古い時代には、テキストベースのプロトコルであるTelnetが使用され、現在ではより高性能なHTTPプロトコルが使用されています。また、別の方法としては、 ```HTTPoison.get``` の代わりにOSのコマンドを呼び出すこともできます。

## 関連情報：
Webページのダウンロードに関する詳細な情報はこちらを参考にしてください。

- [HTTPoison ライブラリ](https://hexdocs.pm/httpoison/readme.html)
- [Elixir の HTTPoison についての記事](https://elixir.com/blog/understanding-httpoison/)
- [Telnetプロトコルについての記事](https://www.cloudflare.com/learning/ddos/glossary/telnet/)
- [OSコマンドの実行についての記事](https://hexdocs.pm/elixir/System.html)