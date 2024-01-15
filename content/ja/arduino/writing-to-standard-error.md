---
title:                "標準エラーに書き込む"
html_title:           "Arduino: 標準エラーに書き込む"
simple_title:         "標準エラーに書き込む"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーへの書き込みをする理由は、エラーをデバッグしたり、プログラミングの問題を特定したりするために必要です。

## 方法

Arduinoの最新版では、`Serial` オブジェクトを使用して標準エラーへの書き込みができます。例えば、`Serial.println()` を使用すると、文字列を標準エラーに書き込むことができます。以下は、`Hello World!` を書き込む例です。

```Arduino
Serial.println("Hello World!");
```

出力結果は、シリアルモニターに表示されます。もし、エラーを書き込む場合は、`Serial.write()` を使用します。例えば、`Error!` を書き込む例です。

```Arduino
Serial.write("Error!");
```

出力結果は、シリアルモニターやシリアルプロットタブに表示されます。シリアルモニターでは、文字列がそのまま表示されますが、シリアルプロットタブでは、文字列を1バイトずつプロットします。

## 深堀り

標準エラーへの書き込みには、`Serial` オブジェクト以外にも方法があります。例えば、`sprintf()` を使用して、文字列をフォーマットしてから標準エラーに書き込むことができます。また、`printf()` を使用して、シリアルプロットタブに文字列をプロットすることができます。詳細な使い方は、ドキュメンテーションを参照してください。

## 関連リンク

- [Arduino リファレンス](https://www.arduino.cc/reference/en/)
- [Serial.println()  ドキュメンテーション](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [sprintf() ドキュメンテーション](https://www.arduino.cc/reference/en/language/functions/character-functions/sprintf/)
- [printf() ドキュメンテーション](https://www.arduino.cc/reference/en/language/functions/communication/serial/printf/)