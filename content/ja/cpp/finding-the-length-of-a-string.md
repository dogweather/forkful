---
title:    "C++: 文字列の長さを見つける"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに取り組む理由は、プログラマーにとって非常に重要です。文字列の長さを知ることで、正しいメモリ領域を確保したり、文字列を比較したりすることができます。

## 方法

文字列の長さを求めるには、標準ライブラリの`strlen()`関数を使用します。以下のコードは、`strlen()`関数を使用して文字列の長さを求める例です。

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    // 文字列の定義
    char str[] = "こんにちは、世界！";
  
    // strlen()関数を使用して文字列の長さを求める
    int length = strlen(str);
  
    // 結果を出力する
    cout << "文字列の長さは" << length << "です。" << endl;
  
    return 0;
}

// 出力: 文字列の長さは13です。
```

このように、`strlen()`関数を使用することで簡単に文字列の長さを求めることができます。

## 詳細解説

`strlen()`関数は、C言語の標準ライブラリの一部です。この関数を使用するには、`cstring`ヘッダーファイルをインクルードする必要があります。また、文字列は終端文字('\0')で終わる必要があります。

`strlen()`関数は、渡された文字列の先頭から終端文字までの間の文字数をカウントします。終端文字自体はカウントされません。そのため、実際の文字数は終端文字を除く長さとなります。

C++において、`std::string`クラスを使用して文字列を扱うことも可能です。このクラスは、`length()`メソッドを使用して文字列の長さを求めることができます。

## 参考リンク

- [C言語標準ライブラリ - strlen()関数](https://www.c-tipsref.com/reference/string/strlen.html)
- [GeeksforGeeks - strlen()関数の使用例](https://www.geeksforgeeks.org/strlen-function-in-c/)
- [std::string::length() - C++ Reference](https://www.cplusplus.com/reference/string/string/length/)