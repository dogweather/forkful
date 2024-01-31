---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:53:44.846076-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ファイル読み込みは、文字通り、テキストファイルから情報を取得する操作です。プログラマーはデータの読み込み、解析、変換、そしてアプリに必要な操作を行うためにこれを行います。

## How to: (方法)
以下に、C言語でテキストファイルを読み込むサンプルコードを示します。

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char line[100];

    file = fopen("example.txt", "r");
    if (file == NULL) {
        perror("ファイルオープンに失敗");
        return EXIT_FAILURE;
    }

    while (fgets(line, sizeof(line), file)) {
        printf("%s", line);
    }

    fclose(file);
    return EXIT_SUCCESS;
}
```

サンプルの出力は、`example.txt` に含まれる内容に依存します。

## Deep Dive (深掘り)
テキストファイルの読み込みは初期のプログラミングから存在します。`fopen()`、`fgets()`、`fclose()` は C 標準ライブラリに備わっており、非常に長い間使われています。代替としてストリームやバッファを用いたり、C++のファイルストリームライブラリを利用することもできますが、C言語の標準はシンプルさが特徴です。

ファイルを開く時は、`fopen()`関数にファイル名とモードを指定します。モードは読み込み("r")、書き込み("w")、追加("a")などがあります。「`fgets()`」はファイルから1行読み込み、終了判断はNULLポインタです。「`fclose()`」はファイルを閉じ、リソースの解放を行います。

エラーハンドリングも大事で、ファイルオープン時にNULLが返された場合はエラーということです。`perror()` はエラーメッセージを出力し、デバッグに役立ちます。

## See Also (関連情報)
- [C Standard Library Reference - fopen](https://en.cppreference.com/w/c/io/fopen)
- [C Standard Library Reference - fgets](https://en.cppreference.com/w/c/io/fgets)
- [C Standard Library Reference - fclose](https://en.cppreference.com/w/c/io/fclose)
- [C File Input/Output](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

この記事を読んで理解が深まったら、それぞれの関数の公式ドキュメントもチェックしてみて下さい。プログラミングは練習が命です。コードをいじって、色々試してみましょう！
