---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Arduinoで部分文字列を抽出する方法

## 何となぜ?
部分文字列の抽出とは、文字列から特定の範囲の文字を取り出すことです。プログラマーは不要な情報を除外し、必要なデータだけに焦点を絞るためにこれを行います。

## 方法:
以下にArduinoの実例を示します。
```Arduino
void setup() {
  String mystr = "こんにちは、Arduino!";
  Serial.begin(9600);
  delay(1000);

  // String.indexOf()を使用して部分文字列を見つけ、その位置を取得します
  int pos = mystr.indexOf('、'); // '、'の位置を探す
  
  // String.substring()を使用して部分文字列を抽出します
  String substring = mystr.substring(0, pos); 
  
  // 取得した部分文字列を出力します
  Serial.println(substring);  //-> "こんにちは"
}

void loop() {
  // nothing here
}
```
上記のコードは、指定の文字位置までの部分文字列を見つけ、それを出力します。

## ディープダイブ:
部分文字列の抽出は、初期のプログラミング言語の設計段階から存在しています。この機能は、ファイル操作、データ解析、ユーザー入力の検証など、様々なケースで便利です。

また、他にも多くの方法で部分文字列を抽出できることを覚えておいてください。たとえば、`String.charAt(index)`を使用することで特定の文字位置を取得できます。しかし、Arduinoでは、「String.substring()」メソッドがよく使用されます。このメソッドはある範囲の文字列を返し、元の文字列には影響を与えません。

## 参考情報:
部分文字列の取り扱いに関して詳しく知りたい場合は以下のリンクが参考になります。

- Arduinoの公式ドキュメント: [Stringオブジェクト](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [文字列処理の基本](http://www.musashinodenpa.com/arduino/ref/index.php?f=1#string_indexof)