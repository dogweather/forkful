---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
テキストファイルを読むとは、プログラムがテキストファイルの内容をメモリに取り込む行為です。「なぜ？」と言えば、プログラマーはテキストファイルを通じてデータを分析、操作、保存するためです。

## ハウツー：
ここではfopenとfgetsを使ってテキストファイルを読み込む基本的なCプログラムを見ていきましょう。
```C
#include <stdio.h>

int main() {
   FILE *file = fopen("sample.txt", "r");
   if (file == NULL) {
       printf("Failed to open file\n"); 
       return 1;
   }

   char line[100]; 
   while (fgets(line, sizeof(line), file)){
       printf("%s", line);
   }

   fclose(file);
   return 0;
}
```
これが`sample.txt`を読むプログラムです。「Failed to open file」が出力されたらファイルが開けなかったことを示します。

## ディープダイブ
テキストファイルを読む行為はコンピュータの初期からあるオペレーションで、歴史的には様々な方法でこの問題が解決されてきました。上の例ではC言語の標準ライブラリの一部であるfopenとfgetsを使いましたが、他にも操作方法は数多く存在します。たとえば、fread、fscanfなどの関数を使う方法もあります。ファイル操作はOSに強く依存しますので、異なるOSでは詳細が異なることを覚えておいてください。

## 参考情報
- The C Programming Language 2nd Edition（Brian W. KernighanとDennis M. Ritchieの著書）: `fopen`や`fgets`に関する詳細情報を得ることができます。
- [C Tutorial – File I/O](https://www.cprogramming.com/tutorial/cfileio.html): より幅広いファイル操作技術を説明しています。