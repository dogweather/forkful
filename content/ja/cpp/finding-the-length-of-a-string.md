---
title:                "C++: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに興味がある理由は、プログラミングにおいて文字列操作が非常に重要だからです。文字列の長さを知ることで、さまざまなテキスト処理が容易になります。

## 方法

文字列の長さを求める方法は様々ありますが、C++言語では`strlen`関数を使用することが一般的です。以下のコードは、文字列の長さを求める基本的な例です。

```C++
#include <iostream>
#include <cstring>

int main() {
    char str[] = "こんにちは、世界！";
    int length = strlen(str); //文字列の長さを求める
    std::cout << "文字列の長さは" << length << "です。" << std::endl;
    return 0;
}
```

上記のコードを実行すると、次のような出力が得られます。

```
文字列の長さは9です。
```

また、`string`型でも`length`関数を使用することで文字列の長さを求めることができます。

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "こんにちは、世界！";
    int length = str.length(); //文字列の長さを求める
    std::cout << "文字列の長さは" << length << "です。" << std::endl;
    return 0;
}
```

同じく、上記のコードを実行すると同じ結果が得られます。

## ディープダイブ

`strlen`関数や`length`関数の原理は、文字列の終端を示す特殊な文字の位置を探索することで、その位置までの文字数をカウントすることによって行われます。また、文字列の長さを求める際には文字コードの扱いにも注意が必要です。

## また見る

- [C++言語の文字列操作](https://ja.wikipedia.org/wiki/C%2B%2B%E8%A8%80%E8%AA%9E%E3%81%AE%E6%96%87%E5%AD%97%E5%88%97%E6%93%8D%E4%BD%9C)
- [stringクラスのメソッド一覧](http://www.cplusplus.com/reference/string/string/)