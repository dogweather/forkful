---
title:    "C++: 文字列の大文字化"
keywords: ["C++"]
---

{{< edit_this_page >}}

# なぜキャピタル化するのか？

C++プログラミングを行っていると、文字列を大文字に変換する必要がある場面がよくあります。例えば、ユーザーが名前を入力する際に、大文字、小文字関係なく正しく処理するためにも、文字列を大文字に変換する必要があります。そのような場合に、stringをcapitalizeすることで、正しい処理を行うことができます。

## キャピタル化の方法

C++で文字列を大文字に変換するには、```toupper```関数を使用します。以下のコードを参考にしてください。

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string str = "hello world";
    for(int i = 0; i < str.length(); i++) {
        str[i] = toupper(str[i]);
    }
    cout << str; // HELLO WORLD
    return 0;
}
```

上記のコードでは、```toupper```関数をループ内で使用して、文字列の各文字を大文字に変換しています。実行すると、"hello world"が"HELLO WORLD"に変換されたことがわかります。

## キャピタル化の詳細

C++の```toupper```関数は、char型の引数を受け取り、大文字に変換した文字を返します。さらに、英字以外の文字は変換されないので、注意が必要です。また、文字列を直接変換するのではなく、新しい文字列に変換した結果を保存する必要があることも覚えておいてください。

## 参考情報

[cplusplus.com - toupper function](https://www.cplusplus.com/reference/cctype/toupper/)

[FreeCodeCamp - 3 Easy Ways to Capitalize a String in C++](https://www.freecodecamp.org/news/create-a-string-and-capitalize-the-first-letter-with-c/)

[Interview Cake - Capitalize Words](https://www.interviewcake.com/question/cpp/capitalize-words)