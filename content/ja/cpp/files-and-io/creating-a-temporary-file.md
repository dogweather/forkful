---
date: 2024-01-20 17:39:55.921862-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.387167-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u4EE5\u524D\u306F\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\
  \u4F5C\u6210\u3059\u308B\u969B\u306B\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\
  \u72EC\u81EA\u306E\u30D5\u30A1\u30A4\u30EB\u540D\u3092\u8003\u3048\u308B\u5FC5\u8981\
  \u304C\u3042\u308A\u307E\u3057\u305F\u304C\u3001\u3053\u308C\u3067\u306F\u5076\u7136\
  \u306B\u30D5\u30A1\u30A4\u30EB\u540D\u306E\u885D\u7A81\u304C\u8D77\u3053\u308B\u30EA\
  \u30B9\u30AF\u3084\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u4E0A\u306E\u554F\u984C\u304C\
  \u3042\u308A\u307E\u3057\u305F\u3002`tmpfile()`\u95A2\u6570\u3084`std::filesystem`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u306B\u3088\u308A\u3001\u3053\u306E\u30D7\u30ED\u30BB\u30B9\
  \u304C\u7C21\u7565\u5316\u3055\u308C\u3001\u5B89\u5168\u306B\u884C\u3048\u308B\u3088\
  \u3046\u306B\u306A\u308A\u307E\u3057\u305F."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## How to: (方法)
```C++
#include <cstdio>
#include <filesystem>

int main() {
    // tmpfile() を使った一時ファイルの作成
    std::FILE* tmpf = std::tmpfile();
    // ファイルへの書き込み
    std::fputs("Hello, World!", tmpf);
    // シークして読み出す
    std::rewind(tmpf);
    char buffer[20];
    std::fgets(buffer, sizeof(buffer), tmpf);
    // 一時ファイルはクローズ時に自動的に削除される
    std::fclose(tmpf);

    // filesystemを使った安全な一時ファイル作成
    std::filesystem::path temp_path = std::filesystem::temp_directory_path() / "example.txt";
    std::ofstream temp_file(temp_path);
    temp_file << "Hello, Filesystem!";
    temp_file.close();
    // 実行結果を表示する前に一時ファイルを削除する
    std::filesystem::remove(temp_path);

    return 0;
}
```

## Deep Dive (掘り下げ)
以前は一時ファイルを作成する際には、プログラマが独自のファイル名を考える必要がありましたが、これでは偶然にファイル名の衝突が起こるリスクやセキュリティ上の問題がありました。`tmpfile()`関数や`std::filesystem`モジュールにより、このプロセスが簡略化され、安全に行えるようになりました。

代替手法としては、一時ファイルの代わりにメモリベースのデータ構造を使用する方法、例えばメモリマップトファイルやデータベースの使用があります。

実装の詳細として、`tmpfile()`はシステムに依存せず使える一方で、`std::filesystem`を使う方法では、ファイルのパスをより細かく制御できます。さらに、一時ファイルの生存期間を明示的に管理することも可能です。

## See Also (参照)
- C++ reference for `tmpfile()`: https://en.cppreference.com/w/c/io/tmpfile
- C++ reference for `std::filesystem`: https://en.cppreference.com/w/cpp/filesystem
- C++ reference for Temporary Files: https://en.cppreference.com/w/cpp/io/c/tmpfile
