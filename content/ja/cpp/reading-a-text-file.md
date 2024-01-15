---
title:                "テキストファイルの読み込み"
html_title:           "C++: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
なぜテキストファイルの読み取りに関する記事を読むのでしょうか？

テキストファイルは、プログラム内でデータを保存するのに非常に便利です。例えば、設定ファイルやユーザーの入力を保存するのによく使われます。テキストファイルを読み取る方法を知っていれば、プログラムにさらに多くの機能を追加することができます。

## How To
```C++
#include <iostream>
#include <fstream>

int main() {
    // ファイルを開く
    std::ifstream file("sample.txt");

    // ファイルの終わりまでループ
    while (!file.eof()) {
        // ファイルから1行読み込んで表示
        std::string line;
        std::getline(file, line);
        std::cout << line << std::endl;
    }

    // ファイルを閉じる
    file.close();
    return 0;
}
```

**出力:**
```
Hello World!
This is a sample file.
```

テキストファイルを読み取るには、ファイルを開くために `<fstream>` ヘッダーファイルを、そしてファイルから1行ずつ読み込むために `std::getline()` 関数を使います。最後に、ファイルを閉じてリソースが正しく解放されるようにします。

## Deep Dive
テキストファイルを読み取る方法は、ファイル入出力ストリームを使うことで行われます。`ifstream` を使うとファイルを読み込み、`ofstream` を使うとファイルを書き込むことができます。また、`fstream` を使うと読み書きの両方ができます。

テキストファイルを読み取る際、`eof()` （End of File）関数が使われます。これはファイルの終わりに到達するまでループするために使われます。また、`getline()` 関数は、ファイルから1行ずつ読み込むことができますが、`get()` 関数を使ってファイルから1文字ずつ読み込むこともできます。

## See Also
関連記事や参考になるリンクを以下に記載します。
- [C++ Reference: ifstream](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ Reference: ofstream](https://www.cplusplus.com/reference/fstream/ofstream/)
- [C++ File Handling](https://www.geeksforgeeks.org/file-handling-c-classes/)