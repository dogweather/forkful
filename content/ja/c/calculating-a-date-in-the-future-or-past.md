---
title:                "C: 将来または過去の日付を計算する"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去の計算をするのは、プログラミングにおいて非常に一般的なタスクです。例えば、次の給料日や友達の誕生日など、特定の日付を知る必要があるためです。C言語を用いて日付を計算する方法を学ぶことで、より複雑な日付計算にも対応できるようになります。

## 方法

まずはC言語で日付を計算する方法を見ていきましょう。以下のコードブロックでは、現在の日付から2週間後の日付を計算する方法を示しています。

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 現在の日付を取得
    time_t now;
    struct tm *tm_now;
    now = time(0);
    tm_now = localtime(&now);

    // 日付を計算するために2週間分の秒数を取得
    time_t two_weeks = 60 * 60 * 24 * 14;

    // 2週間後の日付を計算
    time_t result = now + two_weeks;

    // 表示するフォーマットを指定
    char buffer[80];
    strftime(buffer, 80, "%Y年%m月%d日", localtime(&result));

    // 結果を出力
    printf("2週間後の日付は %s です。\n", buffer);

    return 0;
}
```

上記のコードを実行すると、現在の日付から2週間後の日付が計算されて表示されます。このように、C言語では日付を表すデータ型やライブラリが用意されているので、比較的簡単に日付を計算することができます。

## ディープダイブ

C言語で日付を計算する際には、多くの人が使用する`time.h`ライブラリを知ることが重要です。このライブラリには時刻を表すデータ型や日付を計算するための関数が含まれています。また、日付の計算方法はグレゴリオ暦をベースにしているため、異なる暦を扱う場合には注意する必要があります。さらに、うるう年やタイムゾーンの考慮など、より深い知識が必要になる場合もあります。

## 参考リンク

- [C言語のtime.hライブラリについて](https://www.hackerschool.jp/hs00/time_kb/index.html)
- [日付と時刻を扱う](https://programming-place.net/ppp/contents/c/211.html)
- [C言語の日付と時刻の扱い方](https://qiita.com/m_hayashi/items/eaac3eccbde4615f9bc2)