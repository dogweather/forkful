---
title:                "C++: 文字列の大文字化"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# なぜキャピタルゼーションするのか

プログラミングを行う上で、時には文字列を大文字にする必要が生じます。例えば、特定の文字を検索する際に大文字小文字を区別する必要がある場合や、正しいフォーマットで表示するためには大文字で表記する必要がある場合などです。そのため、C++では文字列をキャピタルゼーションする機能が提供されています。

# キャピタルゼーションの方法

文字列を大文字に変換するには、C++のアルゴリズムを使用します。これを使うことで、簡単に文字列を大文字に変換することができます。以下のコード例を参考にしてください。

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main(){

  // 変換前の文字列を定義する
  string str = "hello world";

  // 大文字に変換する
  transform(str.begin(), str.end(), str.begin(), ::toupper);

  // 変換後の文字列を出力する
  cout << "キャピタルゼーション後の文字列: " << str << endl;

  return 0;
}
```

上記のコードを実行すると、次のような出力が得られます。

```
キャピタルゼーション後の文字列: HELLO WORLD
```

このように、文字列をキャピタルゼーションすることで、大文字に変換されていることが確認できます。

# 深堀り

文字列をキャピタルゼーションする際には、C++の標準テンプレートライブラリの一部である`transform()`関数を使用します。この関数は、指定された範囲の要素を変換するために使用されます。コード例では、`str.begin()`と`str.end()`の間の要素（つまり文字列全体）を大文字に変換しています。

また、`::toupper`はC++の標準ライブラリで定義されている大文字変換用の関数です。このように、C++の標準ライブラリを使用することで、簡単に文字列をキャピタルゼーションすることができます。

# 関連リンク

- [C++ transform()関数の使い方](https://www.studytonight.com/cpp/vector-algorithms/cpp-transform-function)
- [C++ toupper()関数の使い方](https://www.studytonight.com/cpp/vector-algorithms/cpp-toupper-function)