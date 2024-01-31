---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:36.403924-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
Webページをダウンロードするって？ 簡単に言うと、インターネットからコンテンツを取得することだ。なぜプログラマーはこれをするの？ データ分析、ウェブクローリング、バックアップ作成など、色々な理由がある。

## How to: (方法)
```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	url := "http://example.com"
	resp, err := http.Get(url)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	fmt.Println(string(body))
}
```

実行結果のサンプル:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
    ...
</body>
</html>
```

これで`http://example.com`のHTMLがコンソールに表示される。

## Deep Dive (深掘り)
**歴史的背景**: ウェブは元々テキストとハイパーリンクを共有するために作られた。だが今のインターネットはデータの宝庫であり、プログラムで自動的にデータを収集する方法が必要とされた。それがウェブページのダウンロードだ。

**代替手段**: `http.Get`は最もシンプルだが、他にも方法はある。例えば、`http.Client`を使うとタイムアウトを設定したり、リクエストヘッダをカスタマイズしたりできる。

```Go
client := &http.Client{Timeout: time.Second * 10}
req, _ := http.NewRequest("GET", url, nil)
req.Header.Add("User-Agent", "MyCustomUserAgent")
resp, _ := client.Do(req)
...
```

**実装の詳細**: `http.Get`や`http.Client`は内部でTCP接続を開いてHTTPプロトコルに従いサーバーと通信している。Goの標準ライブラリはこれらの処理をカプセル化してくれており、開発者は簡単にデータを扱える。

## See Also (関連情報)
- [Goのhttpパッケージドキュメント](https://pkg.go.dev/net/http)
- [Go言語によるウェブプログラミング](https://golang.org/doc/articles/wiki/)
- [HTTPクライアントの使い方](https://gobyexample.com/http-clients)
