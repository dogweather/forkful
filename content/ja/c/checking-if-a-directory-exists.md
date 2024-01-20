---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "C: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？
ディレクトリが存在するか確認とは、特定のディレクトリが現在のファイルシステムに存在するかどうかをプログラムで調べることです。これは、ファイルの保存やデータの読み取りなど操作前にディレクトリの存在を確認することで、エラーを予防するために行います。

## 方法：
ディレクトリ存在確認の基本的な方法は、`stat`関数を利用することです。
```C
#include <sys/stat.h>
#include <stdbool.h>

bool doesDirectoryExist(char* path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != -1) {
       if (S_ISDIR(statbuf.st_mode)) {
           return true;
       }
    }
    return false;
}

int main() {
    if (doesDirectoryExist("/path/to/dir")) {
        printf("Directory exists!\n");
    } else {
        printf("Directory does not exist!\n");
    }
    return 0;
}
```

このコードの出力は次の通り：
```
Directory does not exist!
```
調べたいディレクトリのフルパスをこの関数に渡します。

## 詳細説明：
ディレクトリ存在確認はUNIX系オペレーティングシステムの初期から存在し、`stat`システムコールによって実装されています。「stat」は「status」の略で、ファイルやディレクトリの状態を調べるために使用されます。`stat`はただし、存在しないパスを指定した場合には-1を返すので、その結果をチェックすることでディレクトリの存在を確認します。

オペレーティングシステムによっては、`access`関数を使ったアプローチもあります。

```C
#include <unistd.h>

bool doesDirectoryExist2(char* path) {
    if (access(path, F_OK) != -1) {
       return true;
    }
    return false;
}
```
しかし、この場合はパスが存在するだけでなく、プログラムが実際にそれにアクセスできるかどうかを確認します。そのため、必ずしもディレクトリが存在することを意味するわけではありません。

## 参考文献：
- [The GNU C Library: Testing File Type](https://www.gnu.org/software/libc/manual/html_node/Testing-File-Type.html)
これらのソースは、`stat`と`access`機能を詳しく議論しています。 深く理解したい場合*にはぜひ参照してください。