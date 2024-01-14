---
title:                "C: テキストファイルの読み取り"
simple_title:         "テキストファイルの読み取り"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むために、読者の皆さんはなぜこの記事を読む必要があるかと思いますか？テキストファイルは、コンピューターを介してデータを保存するための一般的な方法です。プログラミング言語のCでは、テキストファイルを読み取ることができるため、プログラマーにとって非常に重要なスキルです。

## 方法

C言語を使ってテキストファイルを読み込むには、標準ライブラリのファイル操作関数を使用します。まず、テキストファイルを開き、ファイルポインターを取得します。その後、ファイルから1文字ずつ読み込んで、コンソールに出力するプログラムを作成します。

```C
#include <stdio.h>

int main()
{
    // ファイルを開く
    FILE *fp = fopen("sample.txt", "r");

    // ファイルから1文字ずつ読み込む
    char c;
    while ((c = fgetc(fp)) != EOF)
    {
        // コンソールに出力
        printf("%c", c);
    }

    // ファイルを閉じる
    fclose(fp);

    return 0;
}
```

上記のプログラムを実行すると、`sample.txt`ファイルの内容がコンソールに出力されるでしょう。このように、C言語を使って簡単にテキストファイルを読み込むことができます。

## ディープダイブ

テキストファイルは、ASCIIやUTF-8などのエンコーディングによって文字が表現されています。そのため、C言語では`fgetc`関数を使って1文字ずつ読み込みますが、ファイルポインターを移動させることでテキストファイル内の任意の場所から読み取ることもできます。

また、ファイル読み込み中にエラーが発生した場合は、`ferror`関数を使ってエラーチェックを行うことも重要です。

## おわりに

この記事では、C言語を使用してテキストファイルを読み込む方法を紹介しました。テキストファイルの読み込みは、プログラミングにおいて重要なスキルの一つですので、ぜひ参考にしてください。

## 関連リンク

- [C 言語リファレンス](https://ja.wikipedia.org/wiki/C_(プログラミング言語))
- [C 言語入門 (文法・構文 - プログラミングの基礎知識)](https://www.javadrive.jp/cstart/)
- [C 言語プログラミング入門 | GetCreator](https://www.getcreator.com/articles/programming-dt/read-text-with-c)
- [C 言語を使ってテキストファイルを読み込む方法 | Qiita](https://qiita.com/Yarimizu14/items/b5edfab16edc533e7776)