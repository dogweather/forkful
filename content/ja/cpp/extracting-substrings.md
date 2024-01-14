---
title:    "C++: サブストリングの抽出"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出することの重要性は何でしょうか？サブストリングの抽出は、特定の文字列を簡単に処理したいときに便利です。また、文字列の一部を取得して、より複雑なアルゴリズムや機能を実装する際にも必要になることがあります。

## 抽出する方法

まず、サブストリングを抽出するには、`substr()`という関数を使用します。この関数は、`string`クラスに定義されており、抽出したい部分文字列の開始位置と長さを指定することで、サブストリングを取得できます。

以下は、この関数を使用したサンプルコードです。

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "こんにちは、世界！";
    string sub = str.substr(4, 4); // 開始位置: 4, 長さ: 4
    
    cout << sub << endl; // 出力: 世界
    return 0;
}
```

このコードでは、`substr()`関数を使用して、開始位置が4で長さが4の部分文字列を抽出しています。出力結果は「世界」になります。

また、`substr()`関数を使わずに、文字列のインデックスを直接指定して部分文字列を取得することもできます。例えば、`string sub = str.substr(4);`のようにすると、開始位置から最後までの部分文字列が取得できます。

## より詳しい情報

サブストリングの抽出は、文字列の特定の部分を取得するだけでなく、文字列の操作やアルゴリズムの実装にも重要な役割を果たします。例えば、文字列のマッチングや置換においてもサブストリングの抽出は必要です。

また、`substr()`関数では長さを指定する必要がありますが、長さを指定せずに直接終了位置を指定することもできます。`string sub = str.substr(4, 8);`のようにすると、開始位置から終了位置までの部分文字列が取得できます。

## 関連リンク

- [C++ string substr() function](https://www.geeksforgeeks.org/cpp-string-substr-function/)
- [C++ Tutorial: Strings - substr() function](https://www.cplusplus.com/doc/tutorial/string/)