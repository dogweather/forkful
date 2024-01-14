---
title:    "Go: 日付の取得"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ

日付を取得する理由は様々あります。例えば、アプリケーションのログに日付を記録したい場合や、特定の日付を処理する必要がある場合などがあります。Go言語を使えば、簡単に現在の日付を取得することができます。

## 方法

Go言語では、timeパッケージを使用することで日付や時刻を扱うことができます。まず、現在の日付を取得するには、`Now()`関数を使用します。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	fmt.Println(today)
}
```

このコードを実行すると、現在の日付や時刻が表示されます。例えば、2021年4月16日の場合は以下のような結果になります。

```
2021-04-16 10:30:00.000 +0900 JST m=+0.000000000
```

また、特定のフォーマットで日付を取得することもできます。例えば、`Format()`関数を使用すると、指定したフォーマットで日付を取得することができます。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	dateString := today.Format("2006-01-02")
	fmt.Println(dateString)
}
```

このコードを実行すると、以下のような結果になります。

```
2021-04-16
```

詳しくは、[Go言語の公式ドキュメント](https://golang.org/pkg/time)を参照してください。

## ディープダイブ

`time`パッケージには、他にも様々な関数やメソッドが用意されています。例えば、特定の日付を表す`time`オブジェクトを作成したり、日付や時刻の計算を行ったりすることができます。

また、注意すべき点として、Go言語では日付や時刻を扱う際にタイムゾーンの設定が重要です。タイムゾーンが違うと、期待した結果が得られない場合がありますので、注意してください。

## 参考リンク

- [Go言語の公式ドキュメント - time](https://golang.org/pkg/time)
- [A Tour of Go - Time](https://go-tour-jp.appspot.com/basics/15)
- [Goを使ったプログラムから現在日付を操作する方法](https://qiita.com/ushio_github/items/b4eb94dcb761a80aa616)