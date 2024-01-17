---
title:                "はじめまして、3月から協力室所務のスタッフをしています。 Reply文字列の補間"
html_title:           "C++: はじめまして、3月から協力室所務のスタッフをしています。 Reply文字列の補間"
simple_title:         "はじめまして、3月から協力室所務のスタッフをしています。 Reply文字列の補間"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 何 & なぜ？

インターポレーション（文字列挿入）とは、文字列に変数や値を埋め込むことです。これを行う理由は、プログラマーがより柔軟なコードを書くためです。文字列を動的に生成することで、コードをより効率的に実装することができます。

## 方法：

```C++
#include <iostream>
#include <string>

int main() {
    std::string name = "太郎";
    int age = 25;

    std::cout << "私の名前は" << name << "です。" << std::endl;
    std::cout << name << "は" << age << "歳です。" << std::endl;

    return 0;
}
```

```C++
私の名前は太郎です。
太郎は25歳です。
```

## もっと深く：

インターポレーションは、文字列操作の基本的な機能の一つですが、歴史的にはC言語でのprintf関数が使われていました。しかし、C++では簡単な文法を使ってより直感的に文字列を生成することができます。その他の方法としては、文字列結合や文字列フォーマットがありますが、インターポレーションはこれらよりも読みやすく、コードをより簡潔にすることができます。

## 関連リンク：

- [C++の文字列操作についてのチュートリアル](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++のprintf関数についての解説](https://fresh2refresh.com/c-programming/c-string-handling-functions/printf-function-c/)
- [C++における文字列操作のベストプラクティス](https://www.cplusplus.com/forum/general/99718/)