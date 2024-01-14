---
title:                "Go: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラー出力に書き込む理由は、エラーのデバッグやログ出力など、アプリケーションの開発やテストに欠かせないものです。

## 方法

Go言語では、標準ライブラリの`log`パッケージを使用することで標準エラー出力に書き込むことができます。例えば、以下のコードを使用すると、"error occurred"というメッセージが標準エラー出力に書き込まれます。

```Go
package main

import (
	"log"
	"os"
)

func main() {
	log.SetOutput(os.Stderr)
	log.Println("error occurred")
}
```

実行結果は次のようになります。

```
error occurred
```

## ディープダイブ

標準エラー出力に書き込む際には、`log`パッケージの他にも`fmt`パッケージを使用する方法もあります。しかし、`fmt`パッケージではフォーマットされた文字列を標準エラー出力に書き込むため、性能上の問題が発生する可能性があります。そのため、できる限り`log`パッケージを使用することが推奨されています。

## 参考リンク

- https://golang.org/pkg/log/
- https://golang.org/pkg/fmt/
- https://blog.golang.org/stderr-redirect
- https://www.digitalocean.com/community/tutorials/using-logs-to-monitor-your-application-in-go
- https://www.calhoun.io/5-tips-for-logging-in-go/