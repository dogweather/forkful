---
title:    "Gleam: デバッグ出力をプリントする"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# なぜデバッグ出力を行うか

デバッグ出力は、プログラミングにおいて非常に重要です。コードの実行中に何が起こっているかを確認することで、コードの動作を理解し、バグの修正に役立ちます。

## 方法

Gleamでは、デバッグ出力を行うためのシンプルで効率的な方法があります。以下の例を参考にしてください。

```Gleam
import gleam/io

// 変数numを出力する
debug_output("num is {}", [num])

// 複数の変数を出力する
debug_output("numbers are {} and {}", [num1, num2])

// 文字列を出力する
debug_output("hello, world!")

// 条件式を出力する
debug_output("is 10 greater than 5? answer: {}", [10 > 5])

// 配列の中身を出力する
let numbers = [1, 2, 3]
debug_output("numbers: {}", [numbers |. fold("", std/lib/int.to_string, ", ")])
```

上記のコードを実行すると、コンソールに以下のような出力が行われます。

```
num is 5
numbers are 10 and 20
hello, world!
is 10 greater than 5? answer: true
numbers: 1, 2, 3
```

## もっと深く

デバッグ出力はコードのデバッグに役立つだけでなく、コードの理解にも役立ちます。例えば、関数の引数や返り値を出力することで、その関数がどのようなデータを処理しているのかを把握することができます。また、変数の値を出力することで、そのコードがどのような動作をしているのかを把握することができます。

しかし、デバッグ出力を多用すると、コードが複雑になり、可読性が低下する可能性があります。そのため、適度な頻度でデバッグ出力を行うことが重要です。

# 見落としがちなミス

デバッグ出力を行う際には、以下のようなミスに注意しましょう。

- 変数のスペルミスや、間違った変数を出力しているかどうかを確認すること
- 出力文の中で変数を使用する際に、引用符の使用に注意すること
- 配列やタプルなどの複合データ型を出力する際に、正しい方法を使用すること

# See Also

- Gleam公式ドキュメント: https://gleam.run/book/#debug-output
- デバッグ入門: https://www.geeksforgeeks.org/debugging-c-cpp/
- デバッグツールの使い方: https://www.nicovideo.jp/watch/sm35672348