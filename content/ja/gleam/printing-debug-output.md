---
title:                "Gleam: 印刷デバッグ出力"
simple_title:         "印刷デバッグ出力"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力をプリントすることは、プログラマーにとって非常に役立つことがあります。デバッグ出力を使用することで、コードの実行中にどのような値が変数に格納されているかを確認できるため、バグを特定するのに役立ちます。

## 方法

プリントデバッグ出力を使用するには、単純に```Gleam.debug()```を変数の前に置くだけです。以下は、```Gleam.debug()```を使用して変数に値を格納し、その変数をプリントする例です。

```
Gleam.debug(number)
println("The value of the number variable is" ++ number)
```

上記のコードにより、プログラムが実行されると、デバッグログとして「The value of the number variable is [値]」が表示されます。

## ディープダイブ

デバッグ出力は、より複雑なアプリケーションのデバッグにも役立ちます。例えば、ループや条件分岐などの制御フロー内でデバッグ出力を使用することで、プログラムがどのように実行されているかを詳しく確認できます。

また、デバッグ出力には任意の文字列をプリントすることもできます。これにより、特定のプログラムのステップや値を追跡することができます。

## 参考

- [Gleamのデバッグ機能について](リンク1)
- [Gleam公式ドキュメント](リンク2)
- [Gleamコミュニティフォーラム](リンク3)