---
title:    "Go: 将来や過去の日付を計算する"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Why:
## なぜ

計算を行い将来や過去の日付を求めることの意義は何か、1-2文で説明します。

How To:
## 方法

Go言語で日付を計算する方法をコーディング例と共に説明します。サンプルの出力も `` `Go ...」 `` コードブロックを使って示します。

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// 現在の日付
	now := time.Now()

	// 将来の日付を求める
	future := now.AddDate(1, 0, 0) // 1年後
	fmt.Println("1年後の日付は:", future)

	// 過去の日付を求める
	past := now.AddDate(-1, 0, 0) // 1年前
	fmt.Println("1年前の日付は:", past)
}
```

```
// 出力
1年後の日付は: 2022-07-16 16:32:05.61477 +0900 JST m=+31536000.000000000
1年前の日付は: 2020-07-16 16:32:05.61477 +0900 JST m=+0.000000001
```

## ディープダイブ

日付を計算する際のより深い情報を提供します。例えば、 `AddDate()` 関数では年、月、日を指定することで任意の日数を加算・減算することができます。また、 `Time` 型の変数を使用することで、より柔軟な日付の操作を行うことができます。

See Also: 
## 関連リンク
- [Go言語公式ドキュメント（日付と時刻）](https://golang.org/pkg/time/)
- [Go言語での日付操作の基本](https://qiita.com/K_ohkubo/items/52189dd6b3507b215646)
- [Go言語での日付と時刻の処理方法](https://helloworldpark.com/programming/go-time-package/)