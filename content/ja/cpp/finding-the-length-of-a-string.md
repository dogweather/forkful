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

## なぜ

文字列の長さを求めることに関わる理由は、プログラムで文字列を処理する必要があるからです。例えば、ユーザーからの入力を受け付ける場合や、文字列を比較する場合などです。文字列の長さを知ることで、より効率的にプログラムを構築することができます。

## 方法

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 文字列の長さを求める例
    string str = "Hello World";
    int length = str.size();

    cout << "The length of the string is: " << length << endl;

    return 0;
}
```

上記のようなコードを使用することで、文字列の長さを求めることができます。まず、"string"ライブラリをインポートし、"using namespace std;"を使用することで、プログラム内でstringを使用することができます。そして、"size()"関数を使用することで、文字列の長さを取得できます。上記の例では、文字列 "Hello World"の長さが求められ、その結果がコンソールに表示されます。

## ディープダイブ

文字列の長さを求めるためには、プログラムで文字列を扱うにあたっての基本的な知識が必要となります。文字列は、単なる文字の配列としてメモリ上に格納されています。そのため、文字列の長さは、配列の要素数を表す整数として求めることができます。また、C++では、文字列の長さを取得するためにいくつかの関数が用意されています。"size()"関数の他には、"length()"や"capacity()"などの関数もありますので、使用する際には適切な関数を選択することが重要です。

## 関連リンク

- [C++の文字列の長さを取得する方法](https://www.sejuku.net/blog/13606)
- [string - C++ Reference](https://www.cplusplus.com/reference/string/string/)