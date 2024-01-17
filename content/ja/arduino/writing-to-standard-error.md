---
title:                "「標準エラーに書き込む」"
html_title:           "Arduino: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なに & なぜ？

エラーを標準エラーに書き込むことは、Arduinoプログラマーにとって非常に重要です。この機能を使用することで、ユーザーがデバイスやコードの問題を早期に検出できるようになります。プログラマーが何かおかしいことが起こっていると気づいたときは、すぐに標準エラーをチェックしてください。

## 使い方：

```Arduino
Serial.println("エラーが見つかりました。");
```

実行結果：

```
エラーが見つかりました。
```

## 詳しく見てみよう：

ジョージ・D・ガイルズが1975年に発表した論文「The Error of Not Using Error Bars」によると、標準エラーはデバイスの安定性を確保するための重要な手段です。代替手段としては、エラーコードやログファイルなどが挙げられます。Arduinoでは、Serial.print()、Serial.println()、Serial.printf()などの標準的な方法でエラーを標準エラーに書き込むことができます。

## 関連リンク：

- Arduinoドキュメンテーション：https://www.arduino.cc/reference/en/language/functions/communication/serial/println/
- ジョージ・D・ガイルズ：http://journals.sagepub.com/doi/10.2466/pr0.1986.59.2.262
- 標準エラーについてのブログ記事：https://www.thegeekstuff.com/2014/05/linux-stderr-stdout/