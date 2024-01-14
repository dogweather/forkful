---
title:                "C: テキストファイルを読む"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読むか

テキストファイルを読むことは、Cプログラミングで非常に重要なスキルです。テキストファイルには、プログラムに必要なデータが含まれる場合があります。この記事では、テキストファイルを読むための基本的な方法を紹介します。

## 方法

まず、テキストファイルをオープンする必要があります。これには、`fopen()`関数を使用します。以下のコードは、"sample.txt"という名前のテキストファイルをオープンし、ファイルポインターを`fp`という変数にストアする例です。

```C
FILE *fp;
fp = fopen("sample.txt", "r");
```

次に、`fgets()`関数を使用して、ファイルからデータを読み取ります。以下の例では、ファイルから1つの文字列を読み取り、`buffer`という配列に入れています。

```C
char buffer[100];
fgets(buffer, 100, fp);
```

最後に、ファイルをクローズする必要があります。これには、`fclose()`関数を使用します。

```C
fclose(fp);
```

## ディープダイブ

テキストファイルを読むために使用できるさまざまな関数やオプションがあります。例えば、`fscanf()`関数を使用することで、ファイルから特定の形式のデータを読み取ることができます。

また、`while`ループを使用することで、ファイルの終わりまで全てのデータを読み取ることができます。

さらに、ファイルからデータを読み取った後に、そのデータを処理したり、別のファイルに書き込んだりすることも可能です。

## 参考リンク

- [C言語：テキストファイルの読み書き](https://www.javadrive.jp/cstart/file/index1.html)
- [Cでテキストファイルの読み込み・書き込みをする方法](https://techacademy.jp/magazine/13055)
- [C言語の文法：ファイル操作](https://algorithm.joho.info/programming/c/file-operation-c/)
- [【C言語】ファイル操作の基本関数：fopen() fopen_s() fclose()](https://software.fujitsu.com/jp/manual/manualfiles/M1000/BSRE0F14/JIS/01/B1F2O800/11D11700108.html)

## 関連リンク

- [マークダウン記法](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [C言語入門](https://www.javadrive.jp/c-start/)