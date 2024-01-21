---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:52:40.088881-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力はコードのどこがうまく動いているか、どこが動いていないかを見るために使います。プログラマーはこの情報を使って、バグを見つけて修正します。

## How to: (方法)
Gleamでデバッグ出力をするには、`println` 関数を使います。下の例を見てください：

```gleam
pub fn main() {
  let message = "Hello, Gleam!"
  println(message) // コンソールに出力される
}
```

出力：
```
Hello, Gleam!
```

## Deep Dive (深堀り)
デバッグ出力はプログラミングの古典的な技術です。Gleamでは、ErlangのVMを利用していて、`println` 機能はそのシステムに基づいています。ただし、大規模なシステムでのデバッグの際は、ログシステムやエラートラッキングツールを検討する方が良いでしょう。これらはデータをもっと分析しやすくします。

## See Also (関連情報)
- Gleamの公式ドキュメント: [https://gleam.run](https://gleam.run)
- Erlangのログについての詳細：[https://www.erlang.org](https://www.erlang.org)