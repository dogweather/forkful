---
title:                "C: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ作成するのか

一時的なファイルを作成する理由はさまざまです。主な理由は、プログラム上で一時的にデータを保存する必要がある場合です。また、一時的なファイルを使用することでプログラムの実行速度を向上させることができます。

## 作り方

一時的なファイルを作成する方法はいくつかありますが、ここではC言語を使用した方法を紹介します。

```
#include <stdio.h>
#include <stdlib.h>

int main()
{
  // 一時的なファイルの名前を生成する
  char temp_name[20];
  sprintf(temp_name, "temp%d.txt", rand());
  
  // ファイルを作成して書き込む
  FILE *fp = fopen(temp_name, "w");
  fputs("このファイルは一時的なファイルです。", fp);
  fclose(fp);
  
  // ファイルの中身を出力する
  fp = fopen(temp_name, "r");
  char c = fgetc(fp);
  while (c != EOF)
  {
    printf("%c", c);
    c = fgetc(fp);
  }
  fclose(fp);
  
  // ファイルを削除する
  remove(temp_name);
  
  return 0;
}
```

```
このファイルは一時的なファイルです。
```

## さらに深く

一時的なファイルを作成する際には、ファイルの一意性を確保しなければなりません。そのため、ランダムな名前を生成する必要があります。また、ファイルを使用した後は適切に削除することも重要です。

## 参考リンク

- [C言語で一時的なファイルを作成する方法](https://www.codeleading.com/article/70490973809/)
- [一時的なファイルを作成する際の注意点](https://www.ibm.com/support/knowledgecenter/ja/ssw_ibm_i_73/rzai2/rzai2tempfiles.htm)
- [わかりやすく解説！一時的なファイルの使い方](https://note.com/krkoba0216/n/ne62d9ca1984c)