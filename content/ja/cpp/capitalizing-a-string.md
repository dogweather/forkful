---
title:    "C++: 文字列を大文字にする"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーの皆さんは、文字列を大文字にすることが必要な場合があります。例えば、データベースから取得した文字列をタイトルケースに変換する必要がある場合や、入力フォームで入力した文字列を大文字に統一する必要がある場合などです。

## 方法

文字列を大文字にする方法はいくつかありますが、今回はC++プログラミング言語を使用して紹介します。この言語を選んだ理由は、C++は高速でパワフルな言語であり、文字列操作にも向いているからです。

まずは、文字列を大文字に変換するために必要なライブラリをインクルードします。

```
#include <iostream>
#include <string>
```

次に、変換前の文字列を入力します。

```
std::string str = "hello, world!";
```

ここで、文字列を大文字に変換するための関数を定義します。

```
std::string toUpper(std::string str) { 
  for(int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
  }
  return str;
}
```

この関数では、forループを使って文字列の各文字を大文字に変換しています。また、C++の標準ライブラリであるtoupper関数を使用しています。

最後に、変換後の文字列を出力します。

```
std::cout << toUpper(str) << std::endl;
```

実行すると、"HELLO, WORLD!"という文字列が出力されます。

## ディープダイブ

文字列を大文字に変換する方法は、プログラミングにおいて非常に基本的な操作です。しかし、文字列を操作する際には注意が必要です。例えば、日本語のようにマルチバイト文字を含む場合、直接文字列を変換すると文字化けが起きる可能性があります。

また、C++では文字列を表す型としてchar型やchar[]型を使うこともできますが、string型を使用することでより便利に文字列操作を行うことができます。

さらに、文字列の大文字には変換規則があります。たとえば、英語ではタイトルケースとして単語の最初の文字を大文字にする場合もありますが、他の言語では規則が異なる場合もあります。プログラミングする際には、その言語の規則をしっかりと把握する必要があります。

## 併せて読みたい

- [C++ stringクラスの使い方](https://www.javadrive.jp/cpp/string/)
- [C++の文字列操作について](https://rementan.tokyo/2017/11/28/cpp_string_operation/)
- [文字列を大文字に変換するアルゴリズムの比較](https://qiita.com/jin-jin/items/d9a6dc142f968d3cad1c)

see also: 見てね
- [文字列を小文字に変換する方法](https://github.com/miyabihito/capitalize_string/blob/main/capitalize_string.cpp)
- [英語のタイトルケースに変換する方法](https://github.com/takumi1224/title_case/blob/master/title_case.cpp)
- [Pythonで文字列を大文字に変換する方法](https://techacademy.jp/magazine/19386)