---
title:                "ディレクトリの存在をチェックする"
html_title:           "C++: ディレクトリの存在をチェックする"
simple_title:         "ディレクトリの存在をチェックする"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何をするの？なんで？
ディレクトリが存在するかどうかを確認することは、プログラマーがファイルを操作する前にディレクトリが存在するかどうかを確認することです。これにより、予期しないエラーを防ぐことができます。

## 方法：
```
#include <iostream>
#include <filesystem>

using namespace std;
namespace fs = std::filesystem;

int main() {
    // ディレクトリをチェックするパスを指定
    fs::path path_to_check = "example_directory";

    // ディレクトリが存在するかどうかを確認
    if (fs::exists(path_to_check)) {
        cout << "ディレクトリが存在します。" << endl;
    } else {
        cout << "ディレクトリが存在しません。" << endl;
    }

    return 0;
}
```
上記のコードは、C++11以降で利用可能です。ディレクトリをチェックするためには、<iostream>、<filesystem>のヘッダーファイルをインクルードし、std::filesystemの名前空間を使用する必要があります。また、C++17以降ではstd::filesystemの代わりにstd::experimental::filesystemを使用する必要があります。

## 詳しく見る：
ディレクトリの存在を確認するための主な方法は、ディレクトリのパスを指定してfs::exists関数を使用する方法です。しかし、C言語の標準ライブラリやPOSIXシステムコール、Boostライブラリを使用することでもディレクトリの存在を確認することができます。また、例外処理を行わない限り、ディレクトリの存在を確認してもディレクトリのアクセス権限やファイルシステムの制限などは確認されません。

## 関連リンク：
- <https://docs.microsoft.com/ja-jp/cpp/standard-library/filesystem-functions?view=vs-2019>: Visual C++におけるstd::filesystemのドキュメント
- <https://en.cppreference.com/w/cpp/filesystem>: C++のstd::filesystemのリファレンスページ
- <https://www.boost.org/doc/libs/1_77_0/libs/filesystem/doc/index.htm>: Boostライブラリのfilesystemモジュールのドキュメント