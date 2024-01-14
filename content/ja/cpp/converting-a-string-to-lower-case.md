---
title:    "C++: 文字列を小文字に変換する"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# なぜ文字列を小文字に変換する必要があるのか

文字列を小文字に変換することは、プログラムの中でさまざまな理由で必要になることがあります。例えば、ユーザーからの入力を受け取る際に大文字と小文字の区別をしないようにするためや、文字列の比較を行う際に正確な結果を得るために必要です。

## 方法

まずは、文字列を小文字に変換する方法を見ていきましょう。C++では、`std::string`クラスの`tolower()`関数を使用することで簡単に文字列を小文字に変換することができます。

```C++
#include <iostream> 
#include <string> 

int main() { 
    std::string str = "HELLO WORLD"; 
    std::cout << "Before: " << str << std::endl; 

    // 文字列を小文字に変換 
    for (int i = 0; i < str.length(); i++) { 
        str[i] = tolower(str[i]); 
    } 

    std::cout << "After: " << str << std::endl; 

    return 0; 
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
Before: HELLO WORLD 
After: hello world
```

## 深堀り

文字列を小文字に変換する際、いくつかの注意点があります。まず、C++では日本語などのマルチバイト文字を扱う場合に、`tolower()`関数を使用することで意図しない結果を得ることがあります。そのため、マルチバイト文字を扱う場合には`std::wstring`クラスの`tolower()`関数を使用することが、より安全な方法です。

また、文字列を小文字に変換する際には、元の文字列よりも大きいサイズの文字列を用意する必要があります。例えば、元の文字列が10文字であれば、小文字に変換する際には11文字以上のサイズの文字列を用意する必要があります。これは、小文字のアルファベットが元の文字列よりも大きい場合に、文字列のサイズが変わるためです。そのため、十分なサイズの文字列を用意することが重要です。

## 参考リンク

- [C++ reference: std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [C++リファレンス: std::wstring](https://cpprefjp.github.io/reference/string/basic_string/wstring.html)
- [C++ stringのサイズを変更する方法](https://qiita.com/FANTOME_R/items/8fa32202a70ab344e635)

# 参考文献

[C++の文字列を小文字に変換する方法 | TECHSCORE BLOG](https://www.techscore.com/blog/2014/08/19/cpp-string-tolower/)