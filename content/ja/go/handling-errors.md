---
title:                "エラー処理"
date:                  2024-01-26T00:52:54.947007-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/handling-errors.md"
---

{{< edit_this_page >}}

## 何となく？

Goにおけるエラーハンドリングは、実行時の不具合を上手くキャッチして対応することです。プログラムがクラッシュするのを防ぎ、何かが上手くいかなくても予測可能な動作を保証するために行います。

## 方法

Goは明示的なエラーハンドリングを使用します。つまり、関数を呼び出す度にエラーが返されたかどうかをチェックします。例外なしです。以下がその例です：

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("あらら：", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// 何かが間違っていたと仮定
	return fmt.Errorf("何かが間違っている")
}
```

これを実行すると、以下の出力が得られます：

```
あらら：何かが間違っている
```

でも、成功した場合はどうでしょうか？

```Go
func doSomething() error {
	// 今回は全て良好
	return nil
}
```

出力なし。いいですね、無事が良事です。

## 掘り下げ：

Goにおいて、エラーハンドリングは議論の的でした。最初から、Goは例外よりも明示的なアプローチを選択しましたが、これはその単純さを愛する開発者もいれば、冗長だと感じる開発者もいます。組み込みの`error`型はインターフェイスです。`Error() string`メソッドを持つ任意の型がそれを満たします。これはGoの単純さと明示性のエートスに結びついています。

代替手段は？`panic`と`recover`のコンビがありますが、これらはプログラムが続行不可能な場合（言葉通り）に使用されるものです。「panic」は戻ることができないと知っている時に押すイジェクトボタンと考えてください。控えめに使用しましょう。

メインストリームのエラーハンドリングに関しては、Go 1.13がエラーのラッピングを導入し、`errors.Is()`や`errors.As()`のような機能で「エラーチェーン」を簡単に把握することを可能にしました。

## 参照：

Goのエラーハンドリングの全て：

- The Go Blog on Error Handling: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – エラーハンドリングセクション: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Go 1.13 エラーラッピングドキュメント: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Dave Cheneyのエラーハンドリング戦略に関する投稿: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
