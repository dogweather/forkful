---
title:                "一時ファイルの作成"
html_title:           "C: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作ることの意義について、最大2文で説明します。

一時ファイルを作ることは、プログラムの実行中に一時的にデータを保存したい場合や、ファイル操作を行う際に競合を防ぐために利用することができます。

## 作り方

一時ファイルを作るには、C言語で提供されている以下の関数を使用します。

```C
#include <stdio.h>

FILE *tmpfile(void);
```

この関数は、一時ファイルを作成し、そのファイルへのポインタを返します。一時ファイルはプログラムが終了すると自動的に削除されるため、明示的に削除する必要はありません。

また、一時ファイルを作成する際には、`tmpfile()`関数の返り値が`NULL`でないかを確認することが重要です。もし`NULL`であれば、一時ファイルを作成できなかったことを意味します。

以下に、一時ファイルを作成するサンプルコードを示します。

```C
#include <stdio.h>

int main() {
    // 一時ファイルを作成
    FILE *temp = tmpfile();

    // 一時ファイルが作成できたか確認
    if (temp != NULL) {
        printf("一時ファイルが作成されました。\n");
    } else {
        printf("一時ファイルを作成できませんでした。\n");
    }

    return 0;
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
一時ファイルが作成されました。
```

## 深く掘り下げる

一時ファイルを作成する際には、`tmpfile()`関数以外にも、以下のような関数を使用することもできます。

- `tmpnam()`関数：一時ファイルのファイル名を生成する。
- `remove()`関数：一時ファイルを明示的に削除する。

また、一時ファイルを使用する場合は、ファイルへのアクセス権限やディレクトリのパスなども考慮する必要があります。セキュリティ上のリスクを避けるために、十分な確認を行うことが重要です。

## 関連リンク（See Also)

- [C言語入門｜一時ファイルを扱う方法](https://www.javadrive.jp/cstart/file/index7.html)
- [C言語入門サンプル｜一時ファイルの作り方](https://www.k-cube.co.jp/wakaba/server/ftemp.html)
- [C言語の標準ライブラリ：一時ファイルを作成する方法](https://www.codingboo.com/c99/tmpfile.html)