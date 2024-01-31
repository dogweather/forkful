---
title:                "一時ファイルの作成"
date:                  2024-01-20T17:39:55.921862-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/creating-a-temporary-file.md"
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
