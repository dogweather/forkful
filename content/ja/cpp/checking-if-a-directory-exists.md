---
title:                "C++: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することにどのような意義があるのか、簡単にお話ししましょう。ファイルやフォルダーを操作する際に、不必要なエラーを避けるためにディレクトリが存在するかどうかを確認することが重要です。

## 使い方

まずは、ディレクトリが存在するかどうかをチェックする基本的な方法を紹介します。C++のコードブロックを使用して、以下のように記述します。

```C++
#include <iostream>
#include <filesystem> //ファイルシステムライブラリを使用する

namespace fs = std::filesystem; //ファイルシステムライブラリの名前空間を定義する

int main() {
    //チェックするディレクトリのパスを指定する
    fs::path path = "/users/documents";
    //パスが指すディレクトリが存在するかどうかをチェックする
    if (fs::exists(path)) {
        std::cout << "このディレクトリは存在します！" << std::endl;
    }
    else {
        std::cout << "指定したディレクトリは存在しません。" << std::endl;
    }
    return 0;
}
```
上記のコードを実行すると、指定したパスのディレクトリが存在するかどうかが確認され、その結果が表示されます。

## 深堀り

ディレクトリが存在するかどうかを確認するメソッドには、`exists`以外にもいくつかのオプションがあります。例えば、`is_directory`メソッドを使用すると、指定したパスがディレクトリかどうかを確認することができます。また、`is_empty`メソッドを使用すると、指定したディレクトリが空かどうかを確認することができます。

さらに、ファイルシステムライブラリにはディレクトリを作成するためのメソッドもあります。例えば、`create_directory`メソッドを使用すると、新しいディレクトリを作成することができます。また、既存のディレクトリを削除するための`remove`メソッドや、空でないディレクトリを再帰的に削除する`remove_all`メソッドもあります。

## 参考リンク

- [C++ファイルシステムライブラリの公式ドキュメント](https://cpprefjp.github.io/reference/filesystem.html)
- [【C++/Boost】ファイルシステム操作（CASISマガジン）](https://cas-isinc.co.jp/information/boost/filesystem.html)
- [【C++】ファイル操作のさまざまな方法](https://spectruminform.com/2019/09/30/372/)

## 関連リンク

- [C++プログラミング入門](https://w.atwiki.jp/pomi7/pages/52.html)
- [C++入門者向け動画チュートリアル](https://www.youtube.com/watch?v=Na00PAdKtAc)
- [【C++】初めてのファイル入出力](https://qiita.com/kai_kou/items/da09a241459c600d5f04)