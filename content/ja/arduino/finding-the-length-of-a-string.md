---
title:    "Arduino: 文字列の長さを見つける"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ？

プログラミングをしていると、文字列の長さを知ることは非常に重要なことです。文字列の長さを知ることで、文字列の処理や表示に役立つことができます。この記事では、Arduinoで文字列の長さを求める方法をご紹介します。

## 使い方

```Arduino
// 文字列の宣言
String message = "今日はいい天気ですね。";

// 文字列の長さを求める
int length = message.length();

// シリアルモニターに長さを表示
Serial.println(length);
```

### 出力: 16

Arduinoでは、String型の変数に対してlength()メソッドを使うことで、文字列の長さを求めることができます。上記の例では、変数messageに格納された文字列の長さが「16」であることが分かります。

## 詳細を調べる

Arduinoでは、String型の変数に対してlength()メソッドを使うことで、文字列の長さを求めることができますが、実際にはlength()メソッドが使用しているstrlen()関数が重要な役割を果たしています。この関数は、文字列のバイト数を返すため、文字列の全ての文字の長さを正確に求めることができます。

また、ArduinoではUTF-8形式の文字列を使うこともできますが、その場合はlength()メソッドではなく、lengthUTF8()メソッドを使う必要があります。

## 参考リンク

- [ArduinoのStringクラスリファレンス](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/)
- [strlen()関数の詳細](https://www.cplusplus.com/reference/cstring/strlen/)
- [UTF-8形式の文字列に関する情報](https://www.utf8-chartable.de/)