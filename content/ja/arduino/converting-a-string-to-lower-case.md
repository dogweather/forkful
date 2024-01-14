---
title:                "Arduino: 大文字を小文字に変換する"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換する必要性は、プログラミングにおいて文字列を標準化し、比較や検索などの処理を行うためです。

## 方法
Arduinoで文字列を小文字に変換するには、 `toLowerCase()` 関数を使用します。以下のコードを参考にしてください。

```Arduino
String original_string = "THIS IS A STRING";
String lowercase_string = original_string.toLowerCase();
Serial.println(lowercase_string);
```

出力結果：`this is a string`

## 深堀り
`toLowerCase()` 関数は指定された文字列を小文字に変換し、新しいStringオブジェクトを返します。元の文字列は変更されません。また、この関数は英字のみを小文字に変換します。ひらがなやカタカナなど、その他の文字は変換されません。

## See Also
- [Stringオブジェクトのドキュメント](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [toLowerCase() 関数のドキュメント](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/tolowercase/)

記事を読んでいただきありがとうございます。文字列を小文字に変換することで、より効率的なプログラミングが可能になります。ぜひ、この方法を活用してみてください。