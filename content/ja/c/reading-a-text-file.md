---
title:    "C: テキストファイルの読み込み"
keywords: ["C"]
---

{{< edit_this_page >}}

# なぜ

プログラマーの皆さんは、ソフトウェアを作成する際にしばしばテキストファイルを使用します。そのため、テキストファイルを読み取り鞄なには非常に重要なスキルです。この記事では、C言語を用いてテキストファイルの読み取り方を紹介します。

# テキストファイルの読み取り方

C言語を使用してテキストファイルを読み込むには、ファイルポインタを使用します。以下のコードを使用して、ファイルを開き、テキストを読み取ることができます。

```C
#include <stdio.h>

int main() {

    FILE *fp; // ファイルポインタ

    // ファイルを開く
    fp = fopen("sample.txt", "r");

    // ファイルが存在しない場合、エラーメッセージを出力
    if (fp == NULL) {
        printf("ファイルが見つかりませんでした。");
    }

    // ファイルからテキストを読み取り
    char text[100];
    fgets(text, 100, fp);

    // ファイルを閉じる
    fclose(fp);

    // 読み取ったテキストを出力
    printf("%s", text);

    return 0;
}
```

上記のコードでは、`fopen()`関数を使用してファイルを開き、`fgets()`関数を使用してテキストを読み取ります。最後に、`fclose()`関数を使用してファイルを閉じます。このようにして、テキストファイルを読み取ることができます。

# 深堀り

ファイルを読み取る際には、各行の末尾には自動的に改行文字が挿入されます。また、テキストファイルがUTF-8形式でもASCII形式でも、同じ方法で読み取ることができます。

# 関連記事

* [C言語のファイル操作](https://www.javadrive.jp/cstart/file/index1.html)
* [fgets()関数の使い方](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.1.0/com.ibm.zos.v2r1.bpxbd00/rfgets.htm)
* [ファイル入出力の基礎](https://mnishikawa.hateblo.jp/entry/20081017/1224227328)