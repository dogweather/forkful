---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:02.044660-07:00
description: "\u65B9\u6CD5\uFF1A \u73FE\u4EE3\u306EC++\uFF08C++17\u4EE5\u964D\uFF09\
  \u3067\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\
  \u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C7\u30A3\u30EC\
  \u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3092\
  \u542B\u3080\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u64CD\u4F5C\u3092\u5B9F\
  \u884C\u3059\u308B\u305F\u3081\u306E\u76F4\u63A5\u7684\u304B\u3064\u6A19\u6E96\u5316\
  \u3055\u308C\u305F\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.573923-06:00'
model: gpt-4-0125-preview
summary: "\u73FE\u4EE3\u306EC++\uFF08C++17\u4EE5\u964D\uFF09\u3067\u306F\u3001\u30D5\
  \u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\
  \u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u304C\u3067\u304D\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\
  \u5B58\u5728\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3092\u542B\u3080\u30D5\u30A1\
  \u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u64CD\u4F5C\u3092\u5B9F\u884C\u3059\u308B\u305F\
  \u3081\u306E\u76F4\u63A5\u7684\u304B\u3064\u6A19\u6E96\u5316\u3055\u308C\u305F\u65B9\
  \u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
