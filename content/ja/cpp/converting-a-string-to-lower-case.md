---
title:                "文字列を小文字に変換する"
html_title:           "C++: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となく・なぜ？
文字列を小文字に変換するとは、プログラマーが文字列を全て小文字に変換することを指します。プログラマーは、文字列を小文字に変換することで、比較や検索を行いやすくし、データの整合性を保つことができます。

## 方法：
小文字に変換するには、```tolower()```関数を使用します。以下のように使用できます。
```
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    string str = "HELLO WORLD";
    for (char& c: str) c = tolower(c);
    cout << str << endl; // 出力結果：hello world
    return 0;
}
```

## ディープダイブ：
* **歴史的背景：** 文字列を小文字に変換する必要性はプログラミング言語が開発された当初から存在しました。多くのプログラミング言語には、小文字に変換するための組み込み関数があり、これによって文字列処理が容易になりました。
* **代替方法：** 別の方法としては、```transform```関数を使用する方法もあります。これは、STL（Standard Template Library）から提供される関数で、より柔軟に文字列を変換することができます。
* **実装の詳細：** C++の```tolower()```関数は、C言語で定義された```tolower()```関数を基にしています。これは、文字をASCIIコードに基づいて変換するため、ASCIIコード以外の文字（日本語など）には対応していません。

## 関連リンク：
* [ASCIIコードについての詳細](https://ja.wikipedia.org/wiki/ASCII)
* [C++ STLのtransform関数について](https://www.geeksforgeeks.org/transform-c-stl/)