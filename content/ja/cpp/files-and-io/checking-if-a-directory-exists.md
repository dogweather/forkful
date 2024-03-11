---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:02.044660-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:16.122430-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
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
