---
title:                "C++: 部分文字列の抽出"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

こんにちは、プログラマーの皆さん。
今日はC++での抽出サブストリングについてお話しします。

## なぜ

抽出サブストリングは、長い文字列から必要な情報を取り出すために使用されます。たとえば、大きな文章から特定の単語やフレーズを見つけ出す場合に役立ちます。また、入力されたデータからフォーマットを整えたり、特定の部分だけを取り出して処理したりすることができます。

## 方法

C++では、Stringクラスのsubstrメソッドを使って、特定の位置から指定した長さの文字列を抽出することができます。以下のサンプルコードをご覧ください。

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string sentence = "今日はいい天気です。";
  string weather = sentence.substr(4, 3); // 4文字目から3文字分を抽出
  cout << "抽出した情報： " << weather << endl;
  return 0;
}

// Output: 天気
```

上記の例では、"今日はいい天気です。"という文字列から、"天気"という部分を抽出しています。

## 詳細

抽出サブストリングを行う際には、第一引数に指定する位置は文字列の先頭を示す"0"から始まります。また、第二引数に指定する長さの値は省略することもでき、その場合は指定した位置から最後までの文字列が抽出されます。

さらに、C++では文字列の一部分を置換することも可能です。substrメソッドの第三引数に、置換する文字列を指定することで実現できます。

## 関連情報

抽出サブストリングについて、もっと詳しく知りたい方は以下のリンクをご参照ください。

- [C++ substr method documentation](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorialspoint C++ Substr](https://www.tutorialspoint.com/cplusplus/string_substr.htm)
- [GeeksforGeeks String in C++](https://www.geeksforgeeks.org/string-in-cpp/)

それでは、抽出サブストリングを使ってより効率的なプログラミングを行いましょう！