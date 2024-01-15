---
title:                "「ウェブページのダウンロード」"
html_title:           "Go: 「ウェブページのダウンロード」"
simple_title:         "「ウェブページのダウンロード」"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

もし、あなたがインターネット上の特定のウェブページの中身を参照したいとき、Go言語を使ってそのウェブページをダウンロードすることができます。 

## 方法 

ウェブページをダウンロードするために、私たちは `net/http` パッケージを使用します。このパッケージはHTTPクライアントを作成するために使用されます。下のコードを確認してください。

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	request, err := http.NewRequest("GET", "https://www.example.com", nil)
	if err != nil {
		panic(err)
	}

	client := http.Client{}
	response, err := client.Do(request)
	if err != nil {
		panic(err)
	}

	defer response.Body.Close()

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println(string(body))
}
```

このコードを実行すると、ウェブページのHTMLコードが表示されます。 

## 詳細

さらに詳しく使用方法を理解するために、HTTPリクエストやレスポンスについて学ぶことが重要です。HTTPリクエストは、ウェブサーバーに対してどのような動作を行うかを伝えるものです。一方、HTTPレスポンスは、ウェブサーバーからの応答を示します。また、`ioutil` パッケージを使用して、レスポンスボディを文字列として読み込みます。 

## 参考リンク

- [net/http パッケージ](https://golang.org/pkg/net/http/)
- [HTTPクライアントチュートリアル](https://zetcode.com/golang/http/)
- [HTTPリクエストとレスポンスについて](https://www.tutorialspoint.com/http/http_requests.htm)
- [ioutil パッケージ](https://golang.org/pkg/io/ioutil/)