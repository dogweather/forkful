---
title:                "文字列から日付を解析する"
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何してるの & なんで？
日付を文字列から抽出することは、特定の日付や時間を含む文字列からその情報を取り出すことです。プログラマーは、ユーザーから入力されたデータを処理するためによく日付を文字列で受け取りますが、その情報を理解するためには文字列から日付を抽出する必要があります。

## 方法：
Arduinoのプログラムで日付を文字列から抽出するには、 ```parseInt() ```関数を使います。これは、文字列から数字を抽出するための便利な関数です。

例えば、ユーザーが "2021年5月30日" と入力した場合、次のようにコードを書くことで、整数型の変数に "2021" を格納することができます。

```Arduino
String input = "2021年5月30日";
int year = parseInt(input);
```

上記の例では、文字列から日付を抽出する際に ```parseInt() ```関数を使うことで、数字だけを取り出すことができます。

## 深堀り：
日付を文字列から抽出する方法には、さまざまなアプローチがあります。 ``` parseInt() ```関数以外にも、正規表現を使って指定された形式の文字列から日付を抽出することもできます。

日付のパースは、文字列の処理の中でよく使われるテクニックの一つです。また、日付を抽出するためには、文字列の形式を正しく指定することが重要です。

## 関連情報：
以下のリンクを参考にしてください。

- [Arduino公式サイト](https://www.arduino.cc/)
- [日付を文字列から抽出する別の方法](https://www.geeksforgeeks.org/extract-a-number-from-a-string-using-java/)
- [正規表現を使った日付の抽出](https://regexr.com/date-detection.html)