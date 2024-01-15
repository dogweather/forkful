---
title:                "文字列の大文字化"
html_title:           "C++: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の先頭を大文字に変換することがどのような意味を持つのか、その理由を最大2文で説明します。

## 方法

文字列の先頭を大文字にするには、文字列を最初から最後までループし、各文字を大文字に変換します。例えば、"hello world"という文字列を大文字に変換するコードは以下のようになります。

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 変換する文字列を入力
    string str;
    cout << "変換する文字列を入力してください: ";
    getline(cin, str);

    // 文字列の先頭を大文字に変換
    for (int i = 0; i < str.length(); i++) {
        if (i == 0) {
            str[i] = toupper(str[i]);
        }
        else {
            str[i] = tolower(str[i]);
        }
    }

    // 変換後の文字列を出力
    cout << "変換後: " << str << endl;

    return 0;
}
```

出力結果:
```
変換する文字列を入力してください: hello world
変換後: Hello world
```

コードの解説:
- `#include <string>`を追加することで、文字列の操作ができるようになります。
- `getline(cin, str)`を使用することで、コンソールから入力された文字列を取得します。
- `tolower()`と`toupper()`を使用することで、小文字を大文字に変換することができます。
- ループの最初の文字を`toupper()`を使用して大文字に変換し、それ以外の文字を`tolower()`を使用して小文字に変換することで、文字列の先頭を大文字に変換します。

## ディープダイブ

文字列を操作する際には、大文字と小文字の区別が非常に重要です。例えば、"Hello"と"hello"は全く別の文字列として扱われます。そのため、正確なデータの管理や検索をするためにも、文字列の先頭を大文字に変換することはとても役立ちます。

## 他に見るべき

- [C++の文字列操作](https://www.tohoho-web.com/ex/c++/string.html)
- [toupperとtolowerの使い方](https://jp2.php.net/manual/ja/function.toupper.php)