---
title:    "C: 文字列のつなげ方"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの魅力は、複数の文字列を一つにまとめることでプログラムをより効率的に作成できることです。例えば、ユーザーが入力した名前と姓を組み合わせて、新しい名前を作成する場合には文字列の連結が不可欠です。また、文字列を結合することで、プログラムがより柔軟に動作するようにもなります。

## 作り方

文字列を連結するには、プログラムに「string.h」というヘッダーファイルを追加する必要があります。C言語では、文字列は文字の配列として扱われるため、文字列を結合するには配列を結合するイメージを持つことが重要です。以下の例を参考にしてください。

```C
#include <stdio.h>
#include <string.h>

int main() {
    char firstName[10] = "太郎";
    char lastName[10] = "山田";
    char fullName[20] = "";

    strcat(fullName, firstName);
    strcat(fullName, " ");
    strcat(fullName, lastName);

    printf("フルネーム: %s", fullName);

    return 0;
}
```

このコードでは、最初に「string.h」ヘッダーファイルをインクルードし、連結したい3つの文字列を定義します。その後、strcat関数を使用して、文字列を連結し、最終的にフルネームを表示します。

## 深堀り

文字列を結合する方法には様々なアプローチがありますが、一つのポイントは文字列の終端を意識することです。C言語では、文字列の終端を示す「\0」が重要です。そのため、文字列を結合する際には、連結先の文字列の終端に「\0」があることを確認しなければなりません。

また、strcat関数の代わりに、strncat関数を使うことで、一度に結合する文字数を制限することができます。しかし、この場合でも配列の終端に「\0」があるかどうかを注意深く確認する必要があります。

## 関連リンク

[良いプログラムを作成するためのC言語の基礎](https://qiita.com/hirakawahiroshi/items/7f73563f8bc30522741c)

[文字列結合の基礎知識](https://ja.metajack.org/explain/strings/)

[C言語の文字列](https://www.cc.kyoto-su.ac.jp/~yamada/ap/lecture_cpu_c/cpp06.html#sec6)