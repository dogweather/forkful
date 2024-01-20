---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ? - What & Why?

HTTPリクエストの送信は、インターネット上に存在するリソースへの要求を構築し、送信するプロセスです。プログラマーがこれを行う主要な理由は、ウェブページやAPIからデータを取得または送信するためです。

## どうやって: - How to:

PythonでHTTPリクエストを送信する基本的な方法を示します。ここでは、`requests`ライブラリを使用します。

```Python 
import requests

response = requests.get('http://example.com')
print(response.status_code)
print(response.text)
```

上記のスクリプトを実行すると、次のような出力が得られます。

```Python 
200
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```
'requests.get'関数はHTTP GETリクエストを送信します。`status_code`はHTTPステータスコードを、`text`はレスポンスの本文を返します。

## ディープダイブ - Deep Dive

HTTPリクエストは、HTTP（Hypertext Transfer Protocol）の一部であり、1991年のWebの誕生とともに導入されました。Pythonの`requests`ライブラリは、HTTP通信を行うための便利な方法を提供しますが、他のライブラリ、例えば`http.client`（Pythonの標準ライブラリ）や`httplib2`、`treq`などもあります。

内部的には、`requests`が行うことは、TCP/IP接続を開き、HTTPリクエストを構築し、そのリクエストを送信し、サーバからのレスポンスを待つ、という事です。

## 参照リンク - See Also

以下のリンクでは、関連する情報をさらに詳しく学ぶことができます。

- [Python の requests 公式ドキュメンテーション](http://docs.python-requests.org/en/latest/)
- [HTTP についての MDN ガイド](https://developer.mozilla.org/ja/docs/Web/HTTP)
- [Python標準ライブラリ: http.client](https://docs.python.org/3/library/http.client.html)