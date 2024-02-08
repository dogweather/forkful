---
title:                "部分文字列の抽出"
aliases:
- ja/arduino/extracting-substrings.md
date:                  2024-01-20T17:45:13.559736-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

部分文字列を抜き取ることは、文字列から特定の範囲を取り出すプロセスです。これを行う理由は、データ解析、入力のバリデーション、あるいは表示内容のカスタマイズのためです。

## How to: (方法)

```Arduino
String text = "Arduino programming is fun!";
String subText = text.substring(17, 20);

void setup() {
  Serial.begin(9600);
  Serial.println(subText); // Prints "fun"
}

void loop() {
  // Nothing here for now.
}
```

## Deep Dive (深掘り)

Arduino言語では`String`クラスの`.substring()`メソッドを使って文字列から部分文字列を抜き取ります。この機能はJava言語由来で、プログラミング初期からある概念です。代替手段として、Cスタイルの文字配列と関数（`strncpy()`など）を使う方法がありますが、作業はより複雑です。`String`クラスが導入されたことで、高レベルの操作が簡単になりました。しかし、`String`を使うとメモリフラグメンテーションの可能性が増えるため、メモリ使用には注意が必要です。

## See Also (関連情報)

- Arduino公式リファレンス：`String.substring()`に関する詳細 - [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- 同様の操作を行うC言語関数に関する詳細 - [http://www.cplusplus.com/reference/cstring/strncpy/](http://www.cplusplus.com/reference/cstring/strncpy/)
- 文字列操作についての一般的なチュートリアル - [https://www.learncpp.com/cpp-tutorial/c-style-strings/](https://www.learncpp.com/cpp-tutorial/c-style-strings/)
