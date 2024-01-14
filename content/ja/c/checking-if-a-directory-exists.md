---
title:                "C: ディレクトリが存在するかチェックする"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ？

プログラミングをしていると、時には特定のディレクトリが存在するかどうかを知りたい時があります。例えば、あるファイルを書き出す前にそのファイルを保存するディレクトリが存在するかどうかを確認しなければならないかもしれません。そのような場合に、C言語でディレクトリが存在するかどうかを確認する方法についてご紹介します。

## 使い方

まずはじめに、確認したいディレクトリのパスを変数に保存します。次に、`opendir()`関数を使用してディレクトリを開きます。この関数は、成功した場合には`DIR`型のポインタを返し、失敗した場合には`NULL`ポインタを返します。そして、`readdir()`関数を使用してディレクトリ内のファイルを1つずつ読み込みます。最後に、`closedir()`関数を使用してディレクトリを閉じます。

以下に、実際にコーディングしたサンプルとその出力を示します。

```C
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

int main() {
  // ディレクトリのパスを指定
  char *directory = "/Users/username/Demo/";

  // ディレクトリを開く
  DIR *dir = opendir(directory);

  // ディレクトリ内のファイルを1つずつ読み込む
  struct dirent *file;
  while ((file = readdir(dir)) != NULL) {
    printf("File name: %s\n", file->d_name);
  }
  
  // ディレクトリを閉じる
  closedir(dir);

  return 0;
}
```

出力結果:
```
File name: file1.txt
File name: file2.txt
File name: file3.txt
```

## ディープダイブ

ディレクトリが存在するかどうかを確認するには、`opendir()`関数の戻り値が`NULL`かどうかをチェックするという方法が一般的です。しかし、`opendir()`関数はディレクトリが存在しない場合にもエラーを返さない場合があります。そのため、`stat()`関数を使用してディレクトリの属性を取得し、`S_ISDIR()`マクロを使用してディレクトリかどうかをチェックする方法もあります。

## 参考文献

- [opendir(3) man page](https://linux.die.net/man/3/opendir)
- [readdir(3) man page](https://linux.die.net/man/3/readdir)
- [closedir(3) man page](https://linux.die.net/man/3/closedir)
- [stat(2) man page](https://linux.die.net/man/2/stat) 

## 関連情報

- [How to check if a directory exists in C programming?](https://www.programiz.com/c-programming/examples/check-directory-exists)
- [C言語によるファイル・ディレクトリの操作](https://www2.kumagaku.ac.jp/teacher/~hirata/class/C/fileAccess.html)