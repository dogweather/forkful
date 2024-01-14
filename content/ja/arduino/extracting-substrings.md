---
title:    "Arduino: 部分文字列を抽出する"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ

「サブストリングを抽出する」ことについて考えたことはありますか？サブストリングとは、文字列から特定の部分を取り出すことを指します。例えば、ある文書から特定の単語やフレーズを取り出したい場合や、センサーからのデータの特定の部分を取り出したい場合などに使われます。Arduinoでは、サブストリングを抽出することでデータの処理や取得をより簡単にすることができます。

## 方法

サブストリングを抽出するには、Arduinoの ```substring()``` 関数を使用します。この関数には2つの引数があり、一つ目は対象となる文字列、二つ目は抽出したい部分の開始位置を指定します。また、必要に応じて3つ目の引数を指定することで、抽出する文字数を制限することもできます。例えば、次のように使用することができます。

```Arduino
String str = "Hello, World!";
String substr = str.substring(2, 5); // 後ろの3つの文字列を抽出
Serial.println(substr); // "llo"と出力
```

また、文字列中に特定の文字列が含まれているかどうかを判定する ```indexOf()``` 関数を使用することで、より特定の文字列を抽出することもできます。例えば、次のように使用することができます。

```Arduino
String str = "Hello, World!";
int index = str.indexOf("World"); // "World"が最初に出現する位置を取得
String substr = str.substring(index); // index以降すべての文字列を抽出
Serial.println(substr); // "World!"と出力
```

## 深堀り

サブストリングを抽出する際には、文字列の長さや望む部分の位置を正しく指定することが重要です。また、文字列の処理においては、メモリの使用にも注意が必要です。Arduinoでは、文字列処理に ```String``` オブジェクトを使用することが一般的ですが、これらはメモリを消費するため、必要最小限の処理に留めることが望ましいです。

## 関連記事

- [Arduino公式ドキュメント - String.substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino公式ドキュメント - String.indexOf()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)