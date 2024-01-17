---
title:                "文字列の先頭を大文字にする"
html_title:           "C++: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何か？　なぜ？

文字列を大文字にすることは、単純に文字列のアルファベットをすべて大文字に変換することです。プログラマーは、大文字と小文字を区別しなければならない場合や、データベース検索などのために文字列を正規化する必要がある場合に、この処理を行います。

## 方法：

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // 文字列を入力する
  string str = "hello world";

  // 全ての文字を大文字に変換する
  for (int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
  }

  // 出力する
  cout << str << endl;

  return 0;
}
```

出力： HELLO WORLD

## 深堀り：

### 1. 歴史的な文脈

大文字と小文字を区別する必要があるときの問題を解決するため、プログラミング言語やデータベースでは、文字列の大文字化や小文字化のための関数が提供されるようになりました。最も初期のプログラミング言語の一つであるFORTRANでは、大文字と小文字を区別せず、すべての文字を大文字に変換する処理を行う部分がありました。

### 2. 他の解決策

大文字と小文字を区別しないシステムやアプリケーションが開発されるようになったため、大文字と小文字を区別する必要がなくなった場合、文字列の大文字化や小文字化の処理を行う必要はありません。

### 3. 実装の詳細

C++の`string`クラスには、すべての文字を大文字に変換するための`toupper()`関数が用意されています。この関数は、文字のASCIIコードを基に大文字に変換するため、英語以外の言語の文字には対応していません。また、大文字と小文字を区別しないシステムでは、この関数を使わずにすべての文字を大文字にしても問題がありません。

## 関連リンク：

- [toupper()関数のドキュメント](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [FORTRANの仕様書](https://www.fortran.org/technical/details/basic.html)