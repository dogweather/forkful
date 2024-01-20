---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# 文字列の結合: C++での手法とその詳細

## 何となぜ?
文字列の結合とは、複数の文字列を一つにまとめることを指します。これは、出力メッセージを動的に作成したり、データを読みやすくフォーマットしたりするためにプログラマーによく使用されます。

## どうやる:
C++で文字列を結合する基本的な方法を見ていきましょう。

```C++
#include <iostream>
#include <string>

int main() {
    std::string str1 = "こんにちは ";
    std::string str2 = "世界";
    std::string str3 = str1 + str2;

    std::cout << str3 << std::endl;

    return 0;
}
```

上記のコードを走らせると、「こんにちは 世界」と出力されます。

この例では、`+`演算子を使って2つの文字列を結合していますが、`append()` 関数を使うことも可能です。

```C++
std::string str1 = "こんにちは ";
std::string str2 = "ありがとう";
str1.append(str2);
std::cout << str1 << std::endl; 
```

이 코드를 실행하면 "こんにちは ありがとう"가 출력되는 것을 볼 수 있습니다.

## ディープダイブ
文字列の結合はプログラミングの中心的な要素であり、C++の早いバージョンから提供されています。しかし、現在のC++バージョンでは、より効率的にこれを行う方法が提供され、特に大量の文字列を結合する場合にはこれが有用です。

また、代替として `stringstream` クラスもあります。これは、複数のデータ型を一つの文字列に結合するときに役立ちます。

```C++
#include <sstream>

std::ostringstream oss;
oss << "こんにちは, " << "その " << 123;
std::string str = oss.str();
```

この方法はフォーマット操作にも適していますが、純粋な文字列の結合に対しては `+` または `append()` の方が直感的です。

## 参考リンク
1. C++ 文字列リファレンス: https://ja.cppreference.com/w/cpp/string/basic_string
2. `std::stringstream` を使用した結合: https://www.cplusplus.com/reference/sstream/stringstream/stringstream/