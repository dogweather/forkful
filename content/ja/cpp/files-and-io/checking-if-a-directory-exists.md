---
title:                "ディレクトリが存在するかどうかの確認"
aliases: - /ja/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:02.044660-07:00
model:                 gpt-4-0125-preview
simple_title:         "ディレクトリが存在するかどうかの確認"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
ディレクトリの存在を確認するというのは、ファイルの読み書きなどの操作を実行する前に、指定されたパスにディレクトリが存在するかどうかを判断することを指します。プログラマーは、ファイル操作に関連するエラーを避け、アプリケーション内のファイル処理タスクの実行をよりスムーズで信頼性の高いものにするために、これを行います。

## 方法：
現代のC++（C++17以降）では、ファイルシステムライブラリを使用してディレクトリが存在するかどうかを確認することができます。これは、ディレクトリの存在を確認することを含むファイルシステム操作を実行するための直接的かつ標準化された方法を提供します。

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "ディレクトリが存在します。" << std::endl;
    } else {
        std::cout << "ディレクトリは存在しません。" << std::endl;
    }

    return 0;
}
```
ディレクトリが存在する場合のサンプル出力：
```
ディレクトリが存在します。
```

ディレクトリが存在しない場合のサンプル出力：
```
ディレクトリは存在しません。
```

まだC++17を使用していないプロジェクトや追加の機能のためには、同様の機能を提供する人気のサードパーティ製ライブラリであるBoost Filesystemライブラリがあります。

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "ディレクトリが存在します。" << std::endl;
    } else {
        std::cout << "ディレクトリは存在しません。" << std::endl;
    }

    return 0;
}
```
Boost Filesystemを使用した場合、出力は指定されたパスにディレクトリの存在によって、C++17ファイルシステムの例と同一になります。
