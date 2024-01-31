---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

category:             "C"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ファイル作成は情報を保存するプロセスです。データの永続的保存、情報の共有と後で使うために行います。

## How to (方法)
C言語でテキストファイルを書く基本的な方法:

```C
#include <stdio.h>

int main() {
    FILE *filePtr;
    filePtr = fopen("example.txt", "w"); // ファイルを開く

    if (filePtr == NULL) {
        printf("ファイルを開けません。\n");
        return 1;
    }

    fprintf(filePtr, "こんにちは、ファイル！\n"); // テキストを書く
    fclose(filePtr); // ファイルを閉じる

    return 0;
}
```

実行後の`example.txt`の内容:

```
こんにちは、ファイル！
```

## Deep Dive (深堀り)
ファイルへの書き込みは、初期のコンピュータシステムから存在し、データ永続性の基礎を形成します。`fopen()`や`fprintf()`は標準入出力ライブラリにあります。`fwrite()`や`write()`のような低レベル関数も使えますが、直接バイナリデータを扱います。ファイルを開く際の`mode`を変更することで、追加書き込みや読み込み専用モードも選べます。例えば、追加モードは`"a"`で表されます。

## See Also (関連項目)
- C Standard Library - Files: https://en.cppreference.com/w/c/io
- Learn C Programming - Working with files: https://www.learn-c.org/en/Working_with_files
- fopen, fclose, fprintf, and other File Handling functions in C: https://www.geeksforgeeks.org/basics-file-handling-c/
