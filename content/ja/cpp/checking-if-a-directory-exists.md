---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "C++: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

ディレクトリが存在するかを確認するとは、その名が示すように、指定したディレクトリが実際に存在するかを検証することです。エラーの防止、パスの存在保証、データ処理の一貫性維持など、プログラミングにおいて重要な作業です。

## どうやって行うか？ (How to:)

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::filesystem::path dirPath("/path/to/directory");

    if(std::filesystem::exists(dirPath)) {
        std::cout << "The directory exists.\n";
    } else {
        std::cout << "The directory does not exist.\n";
    }

    return 0;
}
```

このコードが実行されると、指定したディレクトリが存在すれば"The directory exists."が、そうでなければ"The directory does not exist."が出力されます。

## ディープダイブ (Deep Dive)

C++でディレクトリの存在確認方法が必要になったのは、ファイルの読み書き操作が一般的になったからです。C++17からは、`<filesystem>`ライブラリを使うことが推奨されています。それまでは`boost::filesystem`か直接OSのAPIを使用する方法が一般的でした。

しかし、いくつかの代替案も存在します。たとえば`boost::filesystem`や`dirent.h`です。これらのヘッダは他の多くのプラットフォームでも利用できます。各ヘッダは、OSやC++のバージョン、さらには使用目的によって選択されます。

## 参考情報 (See Also)

- C++17の`<filesystem>`について: https://en.cppreference.com/w/cpp/filesystem
- `boost::filesystem`について: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
- `dirent.h`について: https://pubs.opengroup.org/onlinepubs/7908799/xsh/dirent.h.html