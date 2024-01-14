---
title:    "Arduino: 正規表現を使用する"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# なぜ正規表現を使うのか
正規表現は、プログラムをより効率的にするための非常に強力なツールです。データやテキストを特定のパターンに一致させたり、検索したり、変更したりすることができます。これにより、データの処理やパターンマッチングが容易になり、プログラムの完成度が向上します。

## 使い方
正規表現を使うには、まずArduinoプログラム内で「REGEX」ライブラリをインクルードする必要があります。その後、パターンに一致させたい文字列を変数に代入し、`Regex`オブジェクトを作成します。例えば、下記のようになります。
```Arduino
#include <Regex.h>
String text = "Hello World!";
Regex regex("World");
```
次に、`match()`関数を使って、文字列がパターンに一致するかどうかを確認することができます。戻り値は`true`または`false`です。
```Arduino
if(regex.match(text)){
    Serial.println("Match found!");
}else{
    Serial.println("No match found.");
}
```
もし、一致した部分文字列を取得したい場合は、`getMatch()`関数を使うことができます。下記の例では、`world`という部分文字列が取得されます。
```Arduino
String match = regex.getMatch();
Serial.println(match); // Output: world
```

## 深く掘り下げる
正規表現にはさまざまなオプションやパターンがあり、それらを使いこなせるようになるとより複雑な処理を行うことができます。たとえば、`match()`関数の代わりに`find()`関数を使うことで、文字列内で複数回パターンに一致する部分文字列を検索することができます。また、正規表現のパターンでは、文字数や文字の種類を制限することができるため、特定の形式に一致するデータを抽出することも可能です。さらに、正規表現のオプションを使うことで、大文字と小文字を区別しない、複数行にまたがる検索を行うなど、検索の柔軟性を高めることができます。

## 参考リンク
- [Arduino正規表現チュートリアル](https://www.arduino.cc/reference/en/regex/)
- [Wokwi Simulatorで正規表現を試してみる](https://wokwi.com/arduino/projects/321338288107616005)
- [正規表現の練習問題](https://regexone.com/) 

# 参考リンク