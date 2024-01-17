---
title:                "一時ファイルを作成する"
html_title:           "C: 一時ファイルを作成する"
simple_title:         "一時ファイルを作成する"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何？　何のため？

一時ファイルを作成するとは、プログラマーが一時的にデータを保存するために作成するファイルのことです。プログラマーが一時ファイルを作成する理由は、プログラム実行中にデータを保持する必要があるからです。

## 方法：

以下のように、C言語で一時ファイルを作成する方法を示します。

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {

    // サイズ100の一時ファイルを作成
    FILE *temp = tmpfile();
    
    if (temp == NULL) {
        printf("一時ファイルの作成に失敗しました。\n");
        exit(1);
    }

    // データを一時ファイルに書き込む
    char data[] = "Hello, world!";
    fwrite(data, sizeof(char), strlen(data), temp);

    // 一時ファイルの内容を読み込んで出力する
    char buffer[100];
    rewind(temp); // ファイルの先頭に移動
    fread(buffer, sizeof(char), 100, temp);
    printf("一時ファイルの内容は: %s\n", buffer);

    // ファイルを閉じて削除する
    fclose(temp);

    return 0;
}
```

出力:

```
一時ファイルの内容は: Hello, world!
```

## 詳細：

### 歴史的な背景：

一時ファイルは、古くからプログラム実行中にデータを一時的に保持するために使用されてきました。例えば、プログラムが大きなファイルを読み込む際に一時的にハードディスクに書き出すことで、メモリを節約するために使われました。

### 代替手段：

一時ファイルには、メモリを使わずにデータを保持することができる代替手段もあります。例えば、プログラム実行中にデータを書き出すための仮想メモリやデータベースを使用することができます。

### 実装の詳細：

一時ファイルは、C標準ライブラリの```tmpfile()```関数を使って作成することができます。これは、プログラムが終了すると自動的に削除されるファイルを作成します。また、```tmpnam()```関数を使うと、ファイル名を指定して一時ファイルを作成することもできます。

## 関連情報：

- [C標準ライブラリのtmpfile()関数のドキュメント](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/tmpfile.htm)
- [一時ファイルについてのサンプルコード](https://www.geeksforgeeks.org/c-programming-tmpfile-function/)
- [一時ファイルの代替手段についての説明](https://stackoverflow.com/questions/974645/what-are-the-advantages-of-using-tmp-file-over-memory-to-store-temporary-data)