---
title:                "将来または過去の日付の計算"
html_title:           "Go: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？

将来または過去の日付を計算するとは、特定の日付から特定の時間単位（日、週、月、年など）を加算または減算することです。プログラマーは、リマインダーをセットしたり、特定の日付に何かをスケジュールしたりするためにこれを行います。

## 方法:

以下にGo言語で将来の日付を計算するコード例とその出力サンプルを示します。

```Go
package main
import (
	"fmt"
	"time"
)
func main() {
	v := time.Now().AddDate(0, 0, 3)
	fmt.Println("今日から3日後の日付は:", v.Format("2006-01-02"))
}
```
このコードの出力は、今日の日付に対して3日後の日付を表示します。

また、過去の日付を計算することも可能です。次のコード例を見てみてください。

```Go
package main
import (
	"fmt"
	"time"
)
func main() {
	v := time.Now().AddDate(0, 0, -3)
	fmt.Println("今日から3日前の日付は:", v.Format("2006-01-02"))
}
```
このコードの出力は、今日の日付に対して3日前の日付を表示します。

## 詳細知識:

(1) 历史背景: 日付計算は古代から行われており、農業、祭り、季節ごとの活動のスケジューリングなど、さまざまな用途で使用されてきました。これらの手法は現代のコンピューターシステムに継承され、日付と時刻の計算は今日のプログラミングにとって基本的な部分となっています。

(2) 代替手段: 上記のコードでは `AddDate` 関数を使用して日付を計算していますが、他にも `Add` メソッドを使用して時間単位（秒、分、時間）で計算することも可能です。

(3) 実装詳細: Goの `time` パッケージは日付と時刻を扱うための豊富な関数とメソッドを提供しています。また、Goでは日付フォーマットに2006-01-02を使用するのが一般的です。

## 参考情報:

- Go公式ドキュメンテーションの `time` パッケージ: https://golang.org/pkg/time/
- `AddDate` メソッドについての詳細: https://golang.org/pkg/time/#Time.AddDate
- `Add` メソッドについての詳細: https://golang.org/pkg/time/#Time.Add