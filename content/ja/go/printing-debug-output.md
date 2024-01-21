---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:52:34.308460-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (なにを？どうして？)
デバッグ出力とは、コードを実行しているところを見たい時に使います。なぜ？バグを見つけて修正したり、動きを理解するためです。

## How to: (やり方)
Goでは、`fmt` パッケージを使って簡単にデバッグ出力できます。サンプルコードを見てみましょう。

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// 標準出力に印刷
	fmt.Println("デバッグ出力を表示")

	// 標準エラーに印刷
	fmt.Fprintln(os.Stderr, "エラー発生時のデバッグ出力")

	// 変数の内容を確認
	debugVar := "確認したい変数"
	fmt.Printf("変数の内容: %v\n", debugVar)
}
```

実行すると、次のような出力が得られます：

```
デバッグ出力を表示
変数の内容: 確認したい変数
```

エラー出力は通常の出力とは異なる場所に表示されることがあります。

## Deep Dive (より深い話)
デバッグの印刷は、多くのプログラミング言語で伝統的なデバッグ手法です。Goが登場する前から、Printfのような関数はプログラマーの定番ツールでした。

代替方法として、ログパッケージやデバッグ専用のツールを使うことがあります。しかし、軽量な作業やサンプルコードでのチェックでは、fmtパッケージで十分です。

詳しい実装については、fmtパッケージのドキュメントを参照すると良いでしょう。出力はOSの標準出力と標準エラー経由で行われますが、これは環境ごとに異なる場合もあります。

## See Also (関連情報)
- Goの公式文書: [fmt package](https://pkg.go.dev/fmt)
- Goの logging パッケージ: [log package](https://pkg.go.dev/log)