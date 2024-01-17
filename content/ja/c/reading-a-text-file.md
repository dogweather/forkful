---
title:                "テキストファイルの読み込み"
html_title:           "C: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

テキストファイルを読み込むとは、プログラマーがコンピューターに保存されたテキストデータを読み込むことを意味します。プログラマーがテキストファイルを読み込む理由は、ファイルに保存された情報をプログラムで処理し、データを利用するためです。

## 方法:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  // ファイルオブジェクトを作成
  FILE *fp;
  
  // 読み込むファイルを開く
  fp = fopen("sample.txt", "r");

  // ファイルが存在しない場合のエラー処理
  if (fp == NULL) {
    printf("ファイルを開けません\n");
    exit(1);
  }

  char buffer[255];

  // ファイルから1行ずつ読み込み、出力する
  while (fgets(buffer, 255, fp) != NULL) {
    printf("%s", buffer);
  }

  // ファイルを閉じる
  fclose(fp);

  return 0;
}
```

**出力：**

```
This is a sample text file.
It contains some data for reading.
```

## 詳細を調べる

- テキストファイルはコンピューターの保存領域の一部であり、プログラムでは単純なテキストエディタとして使用できます。
- 他の方法としては、プログラムでテキストファイルを作成したり、編集したりすることもできます。
- テキストファイルを読み込む実装方法はさまざまですが、基本的な部分は前述のコード例で示したとおりです。

## 関連情報を見る

- [C言語のファイル操作](https://www.javadrive.jp/cstart/file/index1.html)
- [ファイルの読み込みをおえる](https://c-lang.net/tips/file-read.html)