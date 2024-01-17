---
title:                "ウェブページのダウンロード"
html_title:           "Go: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何をするの？なんで？
ネット上でウェブページをダウンロードすることは、ウェブサイトにアクセスしてコンテンツをオフラインで閲覧することです。プログラマーは、オフラインでコンテンツを閲覧したい場合や、プログラムでウェブページのデータを利用したい場合にダウンロードを行います。

## 方法：
```
Goパッケージ "net/http" を使用して、次のようにウェブページをダウンロードすることができます：

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	// URLを指定してウェブページを取得
	resp, err := http.Get("https://example.com")
	if err != nil {
		// エラー処理
		fmt.Printf("Could not get webpage: %v", err)
		return
	}

	// リクエストが成功した場合、レスポンスボディを取得
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		// エラー処理
		fmt.Printf("Could not read response body: %v", err)
		return
	}

	// 出力
	fmt.Printf("Response body: %s", body)
}
```

上記のコードを実行すると、ウェブページのHTMLコードが表示されます。

## 詳細な情報：
ウェブページのダウンロードは、プログラミングにおいてよく行われるタスクの一つです。他の言語にも同様の機能がありますが、Goの場合は「net/http」パッケージを使用することで簡単にダウンロードを行うことができます。また、HTTPリクエストをカスタマイズすることもできます。

## 関連情報：
- [net/httpパッケージドキュメント](https://golang.org/pkg/net/http/)
- [Goでのウェブスクレイピングの方法](https://qvault.io/go/golang-techniques-for-web-scraping/)