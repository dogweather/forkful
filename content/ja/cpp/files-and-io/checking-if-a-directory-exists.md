---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:02.044660-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.573923-06:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u78BA\u8A8D\
  \u3059\u308B\u3068\u3044\u3046\u306E\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\
  \u307F\u66F8\u304D\u306A\u3069\u306E\u64CD\u4F5C\u3092\u5B9F\u884C\u3059\u308B\u524D\
  \u306B\u3001\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\u306B\u30C7\u30A3\u30EC\u30AF\
  \u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u5224\u65AD\
  \u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u306B\u95A2\u9023\u3059\
  \u308B\u30A8\u30E9\u30FC\u3092\u907F\u3051\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u5185\u306E\u30D5\u30A1\u30A4\u30EB\u51E6\u7406\u30BF\u30B9\u30AF\u306E\
  \u5B9F\u884C\u3092\u3088\u308A\u30B9\u30E0\u30FC\u30BA\u3067\u4FE1\u983C\u6027\u306E\
  \u9AD8\u3044\u3082\u306E\u306B\u3059\u308B\u305F\u3081\u306B\u3001\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
