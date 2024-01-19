---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

現在の日付を取得するとは、システムの現在の日付と時間をコードから参照することを意味します。プログラマーはこれを使って、ログのタイムスタンプやレポートの作成日など、タイムリーな情報を生成するために使用します。

## やり方：

以下の Go コードの例をご覧ください：

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("現在の日付:")
	fmt.Println(time.Now().Local())
}
```

このコードを実行すると、以下のような出力が得られます:

```
現在の日付:
2023-11-26 10:17:33.991428 +0900 JST m=+0.000112985
```

## ディープダイブ

Go 言語では、日付や時間についての情報は `time` パッケージによって提供され、このパッケージは標準ライブラリの一部です。これは Go が最初にリリースされたときから存在しています。

代替手段としては、より具体的な現在の日付（例えば、特定のタイムゾーン）を取得するために、`time.Now().In(loc)` などを使用することができます。ここで、`loc` は `time.LoadLocation` を使用して事前にロードされた `time.Location` オブジェクトです。

Go の `time` パッケージは、Unix エポック（1970年1月1日）からの経過時間をナノ秒単位で表す `Time` オブジェクトを利用することで、時間を管理します。`time.Now()` は現在の時刻をこのオブジェクトとして返します。

## 関連情報

- `time` パッケージの公式ドキュメンテーション: https://golang.org/pkg/time/ 
- Go 言語についての詳細: https://golang.org/doc/
- タイムゾーンのロード: https://golang.org/pkg/time/#LoadLocation
- `Time` オブジェクト: https://golang.org/pkg/time/#Time