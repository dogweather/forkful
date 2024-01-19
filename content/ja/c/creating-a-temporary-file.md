---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
一時ファイルの作成とは、一時的にデータを格納することができるファイルを生成することを意味します。プログラマーはこれを行うことで、一時的なデータを扱いやすくなり、またデータ保存のための追加のリソースを必要としないので、パフォーマンスが向上します。

## 実装方法
以下は、一時ファイルを作成し、このファイルにメッセージを書き込み、そしてそのファイルからデータを読み込むCプログラムの例です。

```C
#include <stdio.h>

int main() {
    FILE *tempFile = tmpfile();
    
    // 例として、一時ファイルにメッセージを書き込む
    fprintf(tempFile, "Hello, World!");

    // 一時ファイルの先頭に戻す
    fseek(tempFile, 0, SEEK_SET);
 
    // 一時ファイルから読み込む
    char buffer[20];
    fgets(buffer, 20, tempFile);
    printf("%s\n", buffer);  // "Hello, World!"と表示される
    
    return 0;
}
```

## ディープダイブ
歴史的な文脈としては、一時ファイルの着想はUNIXとその設計哲学から来ています。これは間接的にリソースを扱う方法として用いられます。

代替案としては、一時メモリ領域を使用することもできますが、これは大量のデータを扱うときには制約が出てきます。

一時ファイルに関する実装の詳細としては、多くのCライブラリでは、`tmpfile()`関数は作成した一時ファイルをプログラム終了時または`fclose()`関数呼び出し時に自動的に削除します。

## 関連情報
一時ファイルの更なる情報については以下のリンクをご覧ください:

- [tmpfile()関数 - cppreference.com](https://en.cppreference.com/w/c/io/tmpfile)
- [一時ファイルとは - TechTargetジャパン](https://whatistechtarget-jp.com)
- [UNIXとその設計哲学 - Wikipedia](https://ja.wikipedia.org/wiki/UNIX%E5%93%B2%E5%AD%A6)