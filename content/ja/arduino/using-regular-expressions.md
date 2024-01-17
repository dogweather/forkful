---
title:                "正規表現を使用する"
html_title:           "Arduino: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
正規表現を使用することは、特定のパターンにマッチする文字列を検索するための便利な方法です。プログラマーがこれを行う理由は、文字列やパターンを効率的に比較できるためです。

## 方法：
```
// コード例1:
// キーワード「hello」を含むすべての文字列を検索する。
Arduino ... 

#include<regex.h>

// 文字列を定義する。
String text = "こんにちは、私の名前はJohnです。";

// 正規表現オブジェクトを作成する。
regex_t regex;

// 「hello」を含むパターンを設定する。
regcomp(&regex, "hello", 0);

// 文字列がパターンにマッチするかチェックする。
if(regexec(&regex, text.c_str(), 0, NULL, 0) == 0){
    // マッチする場合は「hello」が含まれるというメッセージを出力する。
    Serial.println("この文字列には「hello」が含まれています。");
}

// 作成した正規表現オブジェクトを消去する。
regfree(&regex);
```

```
// コード例2:
// 指定したパターンにマッチするすべての文字列を置換する。
Arduino ... 

#include<regex.h>

// 文字列を定義する。
String nameList = "John, Bob, Alice, Kate";

// 正規表現オブジェクトを作成する。
regex_t regex;

// 「Bob」という文字列を「Mike」に置換するパターンを設定する。
regcomp(&regex, "Bob", 0);

// 文字列を「Mike」に置換する。
String updatedList = regreplace(nameList.c_str(), regex, "Mike");

// 置換後の文字列を出力する。
Serial.println(updatedList);

// 作成した正規表現オブジェクトを消去する。
regfree(&regex);
```

## 深く掘り下げる：
正規表現は、1960年代から存在している古いテキスト処理方法です。他のパターン検索や置換方法と比較すると、正規表現はより柔軟で強力なツールであると言えます。また、Arduinoはプログラミング言語のC++の一部を使用しており、その言語には既に正規表現の機能が組み込まれています。

## 関連リンク：
- [正規表現の解説 (Wikipedia)](https://ja.wikipedia.org/wiki/%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE)
- [Arduino公式サイト](https://www.arduino.cc/)
- [C++の正規表現の使い方 (cpprefjp)](https://cpprefjp.github.io/reference/regex.html)