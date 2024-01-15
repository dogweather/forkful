---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "C: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認する必要があるのは、プログラマーが自分のコードで特定のディレクトリを取り扱う必要がある場合です。例えば、ファイルを保存する前にディレクトリが存在するかどうかを確認することで、エラーを防ぐことができます。

## 方法

ディレクトリが存在するかどうかを確認するためには、以下のようにプログラムを書きます。

```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
   // 確認したいディレクトリのパスを指定
   char directory_path[100];
   printf("ディレクトリのパスを入力してください：");
   scanf("%s", directory_path); 

   // ディレクトリが存在するかどうかを確認
   struct stat st;
   if (stat(directory_path, &st) == 0) {
       printf("指定されたディレクトリは存在します。\n");
   } else {
       printf("指定されたディレクトリは存在しません。\n");
   }
   return 0;
}
```

上記のプログラムを実行すると、ユーザーにディレクトリのパスを入力するように促され、そのディレクトリが存在するかどうかが確認されます。

## ディープダイブ

ディレクトリの存在確認には、C言語の標準ライブラリであるstat関数を使用します。stat関数は、指定されたパスの情報を取得するためのもので、この情報にはファイルやディレクトリの種類や所有者などが含まれています。stat関数が正常に実行された場合、0が返されます。そのため、ディレクトリが存在するかどうかを確認する際には、この返り値をチェックすることが重要です。

## 参考リンク

- [stat関数の仕様書 (英語)](https://pubs.opengroup.org/onlinepubs/009695399/functions/stat.html)
- [ファイル・ディレクトリの操作 (日本語)](https://marycore.jp/prog/c-lang/file-dir/)