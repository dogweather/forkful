---
title:                "C: 現在の日付を取得する"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ日付を取得する必要があるのか

日付を取得することは、プログラミングにおいて非常に重要です。特定の日付を取得したい場合や、プログラムの実行時の日付を取得したい場合など、さまざまな理由があります。また、日付を取得することで、日付を処理する必要があるデータベースやアプリケーションを作成することができます。

## 使い方

日付を取得する方法は、プログラミング言語によって異なりますが、C言語では標準ライブラリに含まれている「time.h」ライブラリを使用することができます。具体的なコーディング例を以下に示します。

```
#include <stdio.h>
#include <time.h>

int main()
{
    //現在の時刻を取得
    time_t now = time(NULL);
    //整形して出力
    printf("%s", ctime(&now));
    return 0;
}

```

上記のコードを実行すると、次のような出力が得られます。

```
Mon Jan 20 20:54:00 2020
```

これで、現在の日付を取得することができました。また、C言語では日付を取得する他にも、日付を比較するための関数や日付を計算するための関数なども提供されています。

## ディープダイブ

では、深く日付を取得する仕組みを見てみましょう。C言語では、日付や時間を表現するために「time_t」というデータ型を使用します。これは、1970年1月1日午前0時からの経過秒数を表現するもので、整数型の値です。また、プログラム内で日付を表現するためには、構造体「tm」を使用します。

さらに、「time.h」ライブラリには、時間や日付の演算や比較を行うための関数が収められています。詳しくは、公式ドキュメントや参考文献を参照してください。

## 参考リンク

- [time.h ドキュメント](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C言語入門 - 日付を操作する](https://www.javadrive.jp/cstart/date/index6.html)
- [日付・時間関連のライブラリ関数](https://www-asimov.eng.uab.edu/desk_proto/pubs/d_0013.html)

## 関連ページ

- [time.h ライブラリの使用方法](https://www.javadrive.jp/c_start/time/index1.html)
- [C言語のチュートリアル - 日付の演算と比較](https://www.youtube.com/watch?v=C11dJUUqgGQ)