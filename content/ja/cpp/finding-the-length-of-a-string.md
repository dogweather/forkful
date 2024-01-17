---
title:                "文字列の長さを求める"
html_title:           "C++: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
文字列の長さを見つけることは、文字列内の文字数を計算することです。プログラマーたちは、文字列の長さを見つけることで、処理すべきデータの量を把握し、コードの実行を最適化することができます。

## How to:
文字列の長さを見つけるには、```C++``` の ```length``` メソッドを使用します。下の例を参考にしてください。
```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string myString = "Hello World";

    // 文字列の長さを見つける
    int length = myString.length();

    // 結果を出力する
    cout << "文字列：" << myString << endl;
    cout << "長さ：" << length << endl;

    return 0;
}
```
出力結果：
```
文字列：Hello World
長さ：11
```

## Deep Dive:
文字列の長さを見つけるメソッドは、古くからプログラミング言語に存在しています。一つの代表的な方法としては、文字列の最後に ```\0``` という特殊な文字を追加し、その数を数える方法があります。また、C言語では文字列の長さを格納するためのバッファを手動で確保する必要がありましたが、C++では ```string``` クラスが文字列の長さを自動的に管理してくれるため、より簡単に文字列の長さを見つけることができます。

代替手段としては、単純に文字列をループ処理して文字数をカウントする方法や、正規表現を使用してマッチするパターンの文字数をカウントする方法があります。その他にも、文字列の長さを取得するための独自の関数を定義することもできます。

## See Also:
- [C++ string::length() reference](https://www.cplusplus.com/reference/string/string/length/)
- [Introduction to strings in C++](https://www.geeksforgeeks.org/strings-in-c-2/)