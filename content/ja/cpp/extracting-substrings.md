---
title:    "C++: 文字列の抽出"
keywords: ["C++"]
---

{{< edit_this_page >}}

今日はC++プログラミングの抽出する小文字についてお話します。

## なぜ？

抽出する小文字を使用すると、長い文字列から必要な部分だけを取り出すことができます。これは特に、大規模なテキストデータを編集する場合や、特定の文字列を検索する場合に便利です。

## 方法

まずは、テキストを扱うためのstring型変数を作成します。その後、substr()関数を使用して、必要な部分を指定することで、新しい変数に抽出した小文字を保存することができます。下の例では、文字列"Hello World"から"World"という部分だけを抽出するプログラムを示します。

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // テキストを格納する変数を作成
    string text = "Hello World";
    // substr()関数を使用して、"World"を抽出し、新しい変数に保存
    string extractedText = text.substr(6);
    // 結果を表示
    cout << extractedText << endl; // 結果: World
    return 0;
}
```

このように、substr()関数を使用することで、任意の開始位置から指定した長さの部分文字列を抽出することができます。

## 深堀り

substr()関数は、次のような2つの引数を取ります。

```C++
substr(開始位置, 長さ);
```

例えば、上記のプログラムで使用したように、開始位置を省略すると、指定した位置から最後までの文字列を抽出します。また、長さも省略すると、開始位置から最後までの全ての文字列を抽出します。

さらに、substr()関数は、string型以外の変数でも使用することができます。例えば、char型の配列でも使用することができます。

## 参考リンク

- [C++ string::substr()関数](https://www.cplusplus.com/reference/string/string/substr/)
- [C++ string型](https://www.cplusplus.com/reference/string/string/)
- [C++ char型](https://www.cplusplus.com/reference/cstdio/printf/)