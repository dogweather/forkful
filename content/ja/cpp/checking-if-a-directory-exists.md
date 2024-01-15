---
title:                "ディレクトリが存在するかをチェックする"
html_title:           "C++: ディレクトリが存在するかをチェックする"
simple_title:         "ディレクトリが存在するかをチェックする"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ 
ディレクトリが存在するかどうかをチェックすることは、プログラマーにとって非常に重要です。存在しない場合、プログラムが間違った場所で処理を行う可能性があります。

## ハウツー 
```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    // ディレクトリのパスを指定
    fs::path dir_path = "documents/";

    // パスがディレクトリであるかどうかをチェック
    if (fs::is_directory(dir_path)) {
        std::cout << "ディレクトリが存在します" << std::endl;
    } else {
        std::cout << "ディレクトリが存在しません" << std::endl;
    }

    return 0;
}
```
上記のコードの出力は、「ディレクトリが存在します」または「ディレクトリが存在しません」になります。

## ディープダイブ 
ディレクトリが存在するかどうかをチェックするには、標準ライブラリの`<filesystem>`ヘッダーを使用します。`fs::path`がディレクトリのパスであるかどうかを確認するために、`fs::is_directory()`関数を使用します。ディレクトリが存在する場合は、`true`を返し、存在しない場合は`false`を返します。また、`<filesystem>`ヘッダーは、ファイルやディレクトリを作成したり、移動したりするための便利な関数も提供しています。

## 関連リンク 
- [cplusplus.com - std::filesystem::is_directory](https://www.cplusplus.com/reference/filesystem/is_directory/)
- [C++ Reference - filesystem](https://en.cppreference.com/w/cpp/filesystem)