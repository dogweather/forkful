---
title:                "ディレクトリが存在するかどうかを確認する"
date:                  2024-01-19
html_title:           "Bash: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかどうかを確認することは、指定したパスにディレクトリが存在するかどうかチェックするプロセスです。プログラマーは、ファイル操作を行う前にエラーを防ぐためにこれを行います。

## How to: (方法)
C言語でディレクトリの存在をチェックする一般的な方法を示します。

```C
#include <stdio.h>
#include <sys/stat.h>

int doesDirectoryExist(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) 
        return 0; // エラーを表すために0を返す
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char* path = "./exampleDir";
    
    if (doesDirectoryExist(path)) {
        printf("Directory exists: %s\n", path);
    } else {
        printf("Directory does not exist: %s\n", path);
    }
    
    return 0;
}
```

サンプル出力:
```
Directory exists: ./exampleDir
```
または
```
Directory does not exist: ./exampleDir
```

## Deep Dive (深掘り)
ディレクトリの存在をチェックするためには`stat`関数が使われます。これはUNIXに由来する関数で、ファイルの状態を取得します。`stat`が0以外を返した場合、エラーが発生したと見なします。`stat`が成功すれば、`statbuf.st_mode`からディレクトリかどうかをチェックできます。`S_ISDIR`マクロは、モードがディレクトリを表すかどうかを確認します。Cプログラムでは頻繁にファイルシステムに対する操作を行うため、重要なチェックです。

代わりの方法として、`opendir`と`closedir`関数を使用する方法もありますが、一般的に`stat`が推奨されます。そちらはディレクトリを開いて正常に閉じれることを確認することで存在をチェックする方法です。

## See Also (関連情報)
- POSIX `stat` documentation: https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html
- GNU C Library Reference Manual - File Attributes: https://www.gnu.org/software/libc/manual/html_node/File-Attributes.html
- Stack Overflow discussions on directory checking in C: https://stackoverflow.com/search?q=C+check+directory+exists
