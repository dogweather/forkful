---
title:                "テキストファイルの作成"
html_title:           "C: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜ？

テキストファイルを書く理由はたくさんありますが、C言語でプログラミングを学ぶ上では特に重要なスキルです。テキストファイルを読み書きすることで、データの永続性を保つことができます。

## 作り方

テキストファイルを作成するには、以下のようにC言語の ```fopen()``` 関数を使用します。

```C
#include <stdio.h>

int main(){
    // 文字列変数に書き込む内容を定義
    char text[100] = "Hello World!";
    
    // ファイルポインタを指定してファイルを開く
    FILE *file = fopen("sample.txt", "w");
    
    // テキストファイルに文字列を書き込む
    fputs(text, file);
    
    // ファイルを閉じる
    fclose(file);
    
    // プログラムの終了を伝える
    return 0;
}
```

上記のコードを実行すると、 ```sample.txt``` という名前のテキストファイルが作成され、その中に "Hello World!" というテキストが書き込まれます。

## ディープダイブ

テキストファイルを書く際には、ファイルポインタやファイルモードなどの概念を理解する必要があります。また、文字列だけでなく、数値や配列などのデータをファイルに書き込む方法も学ぶ必要があります。さらに、ファイルの読み書きに関するエラー処理も考慮する必要があります。

# 参考

- [C言語のファイル入出力](https://www.javadrive.jp/cstart/file/index1.html)
- [C言語入門45 複数のデータ構造を扱う](https://www.masanoribrand.site/entry/2017/11/10/170620)
- [C言語のエラーハンドリング](https://www.juce.com/doc/tutorial_handling_errors)