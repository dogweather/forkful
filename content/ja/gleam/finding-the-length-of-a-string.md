---
title:                "Gleam: 文字列の長さを見つける"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

###なぜ
文字列の長さを求めることに関わる理由を1-2文で説明します。

Stringの長さを求めることは、プログラミングで非常に一般的なタスクです。文字列の長さを知ることで、プログラム内で特定の操作を行うことができます。たとえば、テキストをチェックする際に、入力された文字列の長さを確認することができるでしょう。

###方法
以下の「Gleamコードブロック」内で、コーディングの例と出力のサンプルを示します。

```Gleam
let string = "こんにちは"
let length = String.length(string)
// 文字列の長さを求めるためのString.length関数を使用します
IO.println(length)
// 出力: 5
```

String.length関数を使用することで、簡単に文字列の長さを求めることができます。これを活用することで、さまざまなプログラミングタスクをより効率的に行うことができるでしょう。

###深堀り
String.length関数の背後にある原理についても少し深く掘り下げてみましょう。

文字列は、コンピュータ上ではバイト列として表現されます。言い換えると、文字列内の各文字は1バイトのバイナリデータとして保存されています。そのため、文字列の長さを求める際には、バイト数をカウントすることで解決することができます。GleamのString.length関数は、内部的にはバイト数をカウントしているのです。

###参考
［Gleam公式ドキュメント］(https://gleam.run/documentation/strings)
［Gleamで文字列を操作する方法］（https://medium.com/@astrongauthor/working-with-strings-in-gleam-44b488832fda）