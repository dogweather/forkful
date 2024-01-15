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

## Why 
読み込みや書き込みが容易であり、さまざまなデータを処理するのに便利なテキストファイル。C言語でテキストファイルを読み込む方法を学べば、プログラミングのスキルを向上させることができます。

## How To
```C
#include <stdio.h> //標準入出力ファイルの読み込みに必要なヘッダーファイル

int main()
{
    FILE *fp; //ファイルポインタの宣言
    char c; //文字型変数の宣言
    
    //ファイルを読み込みモードでオープン
    fp = fopen("sample.txt", "r");
    
    //ファイルの終端に達するまで1文字ずつ読み込む
    while ((c = fgetc(fp)) != EOF)
    {
        printf("%c", c); //読み込んだ文字を出力
    }
    
    //ファイルをクローズ
    fclose(fp);
    
    return 0;
}
```

**コードの説明**: まず、`stdio.h`ヘッダーファイルをインクルードして標準入出力関数を使えるようにします。次に、`main()`関数の中でファイルポインタを宣言し、`fopen()`関数を使ってテキストファイルを読み込みモードでオープンします。`while`ループを使ってファイルの終端に達するまでファイルから1文字ずつ読み込み、`printf()`関数を使って出力します。最後に、`fclose()`関数を使ってファイルをクローズし、プログラムを終了します。

**サンプル出力**: `sample.txt`ファイルには以下のテキストが記載されているとします。

```
Hello World!
This is a sample text file.
```

このプログラムを実行すると、以下のような出力が得られます。

```
Hello World!
This is a sample text file.
```

## Deep Dive
テキストファイルを読み込む際には、`fgetc()`関数を使って1文字ずつ読み込む方法以外にも、`fgets()`関数を使って一行ずつ読み込む方法や、`fread()`関数を使って一度に指定したバッファーサイズだけ読み込む方法もあります。また、ファイルの書き込みについても`fprintf()`や`fwrite()`などの関数を使って行うことができます。さらに、テキストファイル以外にもバイナリファイルを読み込んだり書き込んだりすることもできます。

## See Also
- [C言語でファイル操作をする方法](https://www.sejuku.net/blog/52004)
- [ファイル操作（ファイル入出力）の基礎 - 終わりを示す「EOF」](https://www.cc.kyoto-su.ac.jp/~yamada/programming/c/file_io.html)
- [C言語プログラミング講座 第24回 ファイル入出力](http://www9.plala.or.jp/sgwr-t/c/sec24.html)