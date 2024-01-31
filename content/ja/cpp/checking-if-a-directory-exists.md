---
title:                "ディレクトリが存在するかどうかを確認する"
date:                  2024-01-19
html_title:           "Bash: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
存在チェックは、ディレクトリが存在するかどうかを確認するプロセスです。プログラマはこれを行うことで、ファイル操作がエラー無く動作するかを事前に確認できます。

## How to: (方法)
```C++
#include <iostream>
#include <filesystem>

int main() {
    std::string path = "/path/to/directory";
    if (std::filesystem::exists(path)) {
        std::cout << "Directory exists: " << path << std::endl;
    } else {
        std::cout << "Directory does not exist: " << path << std::endl;
    }
    return 0;
}
```

サンプル出力:
```
Directory exists: /path/to/directory
```
または
```
Directory does not exist: /path/to/directory
```

## Deep Dive (深掘り)
ディレクトリの存在チェックは、以前は `stat` 関数や `opendir()` 関数を使用していましたが、C++17からは `std::filesystem` ライブラリを使って簡単に行えるようになりました。`std::filesystem::exists` 関数は簡潔にこのチェックを行えますが、ファイルが存在するかどうかだけでなく、それがディレクトリかどうかも確認できます。この機能は、プログラムがファイルシステムの特定の場所の状態に依存する場合に特に有用です。ただし、この呼び出しはファイルシステムへアクセスするため、頻繁に使用するとパフォーマンスに影響を与えることがあります。

## See Also (関連情報)
- C++17 `std::filesystem` リファレンス: https://en.cppreference.com/w/cpp/filesystem
- ファイルシステム操作のチュートリアル: https://www.learncpp.com/cpp-tutorial/working-with-directories/
- `boost::filesystem`（C++17以前の代替手段）ドキュメンテーション: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
