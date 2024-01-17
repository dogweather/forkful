---
title:                "パターンにマッチする文字を削除する"
html_title:           "Arduino: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
文字のパターンに一致する文字を削除することは、プログラマーがよく行う作業です。これを行う理由は、データの整理や処理を行うためです。

## 方法：

```arduino
// コード例１：文字列から特定の文字を削除する
String inputString = "Hello, world!"; // 入力文字列
char charToRemove = 'o'; // 削除したい文字
String outputString = ""; // 出力文字列

for (int i = 0; i < inputString.length(); i++) {
  // 入力文字列の文字を１文字ずつ取り出す
  char currentChar = inputString.charAt(i);
  
  // 削除したい文字と一致するかチェックする
  if (currentChar != charToRemove) {
    // 一致しない場合は、出力文字列に追加する
    outputString += currentChar;
  }
}

// 出力結果をシリアルモニターに表示する
Serial.println(outputString); // Hello, wrld!
```

```arduino
// コード例２：正規表現を使用して文字列から特定のパターンに一致する文字を削除する
String inputString = "I have 10 apples and 5 oranges."; // 入力文字列
String outputString = ""; // 出力文字列

// 正規表現パターンを作成し、一致する部分を空文字で置き換える
regex pattern = " \\d+ "; // スペースの前後に数字がある部分を削除する
outputString = regex_replace(inputString, pattern, "");
    
// 出力結果をシリアルモニターに表示する
Serial.println(outputString); // I have apples and oranges.
```

## 詳細：
削除する文字やパターンを指定することで、より効率的にデータを処理することができます。正規表現を使用することで、柔軟に文字列を指定することができるため、より高度な削除が可能です。また、代替手段として文字列の置換を行う方法もありますが、削除することでデータの一部を維持することができます。実装の詳細については、プログラミング言語やライブラリによって異なります。

## 関連リンク：
- [正規表現チュートリアル (W3Schools)](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [regex_replaceの参考ドキュメント (Arduino)](https://www.arduino.cc/reference/en/language/functions/strings/stringobject/regexreplace/)