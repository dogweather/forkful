---
title:                "デバッグ出力の印刷"
html_title:           "Gleam: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

何と、なぜ？ 
デバッグ出力のプリントとは何かを説明するために、プログラマーが行う理由を二、三文で説明します。

## 何と、なぜ？ 

デバッグ出力のプリントとは、プログラマーがコードの実行中に発生するエラーや異常を特定し、修正するために画面にメッセージを表示することです。プログラマーは、デバッグ出力を使用することで、プログラムが正しく動作していることを確認できます。

## ハウツー： 

```Gleam
fn main() {
  let name = "John";
  debug_print("Hello, {}!", name);
}
```

実行結果：
```
Hello, John!
```

```Gleam
struct Circle {
  radius: Float,
  area: Float,
}

impl Circle {
  pub fn new(radius: Float) -> Circle {
    let area = circle_area(radius);
    debug_print("Area of circle with radius {} is {}.", radius, area);
    Circle { radius, area }
  }
}

fn circle_area(radius: Float) -> Float {
  3.14 * pow(radius, 2.0)
}
```

実行結果：
```
Area of circle with radius 5.0 is 78.5.
```

## ディープダイブ： 

デバッグ出力は、プログラミングの歴史的な文脈では欠かせないものでした。コンピューターが高価で、メモリやプロセッサーの能力が限られていた時代では、デバッグ出力はプログラムの実行中に表示する唯一の方法でした。現在でも、デバッグ出力はコードの動作を確認するために大切な手段です。

代替手段として、プログラマーは特殊なデバッガーやログファイルを使用することもできます。しかし、デバッグ出力はより簡単で直接的な方法でエラーや異常を特定し、修正することができます。

デバッグ出力を実装する方法はプログラミング言語によって異なりますが、多くの言語では````print````や````printf````といった関数を使用します。Gleamでは、````debug_print````関数が使えます。

## 関連情報を確認する： 

- [Gleamの公式ドキュメント](https://gleam.run/getting-started/#Printing).
- [デバッグについての記事-TOAST](https://taosmi.hatenablog.com/entry/2016/08/02/153612).
- [デバッグ出力についてのQ&A-Stack Overflow](https://stackoverflow.com/questions/6454185/what-is-debug-print).