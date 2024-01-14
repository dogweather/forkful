---
title:    "Arduino: 文字列の先頭を大文字にする"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ

プログラマーの皆さんこんにちは、今日はArduinoで文字を大文字にする方法について紹介します。文字を大文字に変換する理由は何でしょうか？たとえば、ユーザーが入力した文字列を処理して、一部の文字を大文字に変換して表示したい場合などに使われます。

## 作り方

大文字に変換するには、Arduinoの`String`オブジェクトで提供される`toUpperCase()`関数を使用します。以下のサンプルコードを参考にしてください。

```Arduino
String inputString = "This is a Sample String";
String capitalizedString = inputString.toUpperCase();
Serial.println(capitalizedString);
```

上記のコードを実行すると、シリアルモニターには「THIS IS A SAMPLE STRING」という出力が表示されます。`toUpperCase()`関数は元の文字列の変更を行わず、大文字に変換した新しい文字列を返すことに注意してください。

## 詳しく見る

大文字に変換する方法はさまざまありますが、Arduinoでは`String`オブジェクトの他にも、`toupper()`関数や`strlwr()`関数を使用する方法もあります。これらはC言語でよく使われる関数で、詳細はグーグル先生にお任せいただきたいです。ただし、Arduinoでは文字列処理を行う際にはメモリの節約のために`String`オブジェクトを使用することが推奨されています。

## 参考資料

- Arduinoの`String`クラスのドキュメント: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- 関数`toupper()`のドキュメント: https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm
- 関数`strlwr()`のドキュメント: https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm

## 関連リンク

- [Arduinoで文字列を操作する方法](https://arduinogetstarted.com/reference/arduino-string-object)
- [Arduinoのライブラリで文字列操作を強化する](https://www.arduino.cc/en/Reference/StringObject)
- [文字列操作の基礎知識](https://www.tutorialspoint.com/c_standard_library/string_h.htm)

ありがとうございました。これで皆さんも文字を大文字に変換する方法をマスターできましたね！