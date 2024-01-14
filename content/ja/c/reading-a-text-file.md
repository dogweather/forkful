---
title:    "C: テキストファイルの読み込み"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むのにはなぜ興味を持つのでしょうか？テキストファイルを読み込むことで、プログラミング言語の基本的な概念を学ぶことができます。また、テキストファイルを読み込むことで、プログラムが外部のデータとどのようにやり取りするのかを理解することができます。

## 方法

テキストファイルを読み込む方法を説明します。まずは、ファイルを開くための標準的なC言語の関数である`fopen`を使用します。次に、`fgets`関数を使用して、ファイルから1行ずつ読み込みます。最後に、読み込んだデータを処理して、必要な処理を行います。

```C
#include <stdio.h>

int main() {
    FILE *fp;
    char buf[256];

    fp = fopen("sample.txt", "r"); // ファイルを読み込み用に開く
    if (fp == NULL) { // ファイルを開けなかった場合
        printf("ファイルを開けませんでした。");
        return 1;
    }

    while (fgets(buf, 255, fp) != NULL) { // 1行ずつ読み込み
        printf("%s", buf); // 読み込んだデータを出力
    }

    fclose(fp); // ファイルを閉じる
    return 0;
}
```

上記の例では、`sample.txt`という名前のテキストファイルを読み込み、その内容をコンソールに出力します。ファイルを開く際は、`fopen`関数でファイルを開き、`fclose`関数でファイルを閉じることを忘れないようにしましょう。

## ディープダイブ

テキストファイルを読み込む際には、文字コードの問題に注意する必要があります。例えば、日本語のテキストファイルを読み込む際は、文字コードがUTF-8であることを指定する必要があります。そうでないと、文字化けが起こる可能性があります。

また、テキストファイルを書き込む際には、ファイルのモードを`w`とすることで、既存のファイルを上書きしたり、新しいファイルを作成したりすることができます。

## 他に読んでみる

- [C言語でテキストファイルを読み込む方法](https://beginnersbook.com/2014/01/c-program-to-read-a-file/)
- [fgets関数の詳細](https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm)
- [fopen関数の詳細](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)