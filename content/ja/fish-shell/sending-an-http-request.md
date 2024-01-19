---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

---
## 何となぜ？

HTTPリクエストの送信とは、特定のウェブリソースへのリクエストを意味します。プログラマーはこれを用いて、データを取得・送信、または特定のリモートサーバーとのコミュニケーションを実現します。

## やり方：

Fish ShellでHTTPリクエストを送信する一例をご紹介します。`curl`コマンドを使います。

```Fish Shell
function http_get
  set url $argv[1]
  curl $url
end
```

例えば、Googleのホームページの情報を取得する場合、以下のコマンドを入力します。

```Fish Shell
http_get https://www.google.com/
```

このスクリプトは指定したURLの内容を画面に表示します。

## ディープ・ダイブ：

HTTPリクエストの送信が最初に提唱されたのは、1990年代初頭のWorld Wide Webの誕生とともにです。それ以来、この技術はウェブのコアな部分となり、今日のインターネットの基盤を支えています。

Fish Shellでは`curl`のようなコマンドを使う以外にも、HTTPリクエストを送信する方法がいくつかあります。例えば`wget`や`httpie`コマンドもよく使われます。これらの方法を選ぶ際は、使いたい機能や好みによります。

Fish ShellにおけるHTTPリクエストの実装詳細については、POSIXベースのシェルと同様であり、BashやZshといった他のシェルと比較して大きな違いはありません。

## 参照：

以下のリンクは、Fish ShellとHTTPリクエストに関する有用な情報源です。

- Fish Shell公式ドキュメンテーション： [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- HTTPリクエストについての詳細： [https://developer.mozilla.org/ja/docs/Web/HTTP/Methods](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)
- `curl`コマンドについての詳細： [https://curl.haxx.se/docs/manpage.html](https://curl.haxx.se/docs/manpage.html)