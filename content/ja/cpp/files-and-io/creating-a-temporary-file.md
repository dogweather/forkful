---
date: 2024-01-20 17:39:55.921862-07:00
description: "\u4F5C\u6210\u3059\u308B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u3001\
  \u30C7\u30FC\u30BF\u3092\u77ED\u671F\u9593\u4FDD\u6301\u3059\u308B\u305F\u3081\u306E\
  \u30D5\u30A1\u30A4\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u30C7\u30FC\u30BF\u3092\u4E00\u6642\u7684\u306B\u51E6\u7406\u3001\u307E\u305F\u306F\
  \u5206\u6790\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u91CD\u3044\u5165\u51FA\
  \u529B\u64CD\u4F5C\u3092\u6E1B\u3089\u3059\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.535612-07:00'
model: gpt-4-1106-preview
summary: "\u4F5C\u6210\u3059\u308B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u3001\
  \u30C7\u30FC\u30BF\u3092\u77ED\u671F\u9593\u4FDD\u6301\u3059\u308B\u305F\u3081\u306E\
  \u30D5\u30A1\u30A4\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u30C7\u30FC\u30BF\u3092\u4E00\u6642\u7684\u306B\u51E6\u7406\u3001\u307E\u305F\u306F\
  \u5206\u6790\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u91CD\u3044\u5165\u51FA\
  \u529B\u64CD\u4F5C\u3092\u6E1B\u3089\u3059\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\
  \u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## What & Why? (なぜとは？)
作成する一時ファイルは、データを短期間保持するためのファイルです。プログラマーはデータを一時的に処理、または分析するため、または重い入出力操作を減らすために使います。

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
