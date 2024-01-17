---
title:                "「未来または過去の日付の計算方法」"
html_title:           "C: 「未来または過去の日付の計算方法」"
simple_title:         "「未来または過去の日付の計算方法」"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

何 & なぜ?

日付を未来や過去の特定の日数分計算することは、プログラマーにとって便利なことです。これにより、日付を柔軟に操作することができます。例えば、ファイルの有効期限を計算する必要がある場合などに役立ちます。

方法:

```C 
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    int days = 5; //計算したい日数
    time_t now = time(NULL); //現在時刻を取得
    struct tm *future = localtime(&now); //現在時刻を構造体に変換
    future->tm_mday += days; //日付を計算
    char output[50]; //出力用の文字列
    strftime(output, sizeof(output), "%Y/%m/%d", future); //構造体を文字列に変換
    printf("Future date: %s\n", output); //結果を表示
    
    return 0;
}
```

出力:

```
Future date: 2021/01/06
```

深堀り:

プログラミング言語によっては、日付を計算するための専用の関数が用意されている場合もあります。また、ライブラリを使用することで、より複雑な計算やタイムゾーンの考慮が可能になります。

さらに、日付を計算する際には、閏年や月の日数などを考慮する必要があります。計算結果が正しいものであることを確認するためには、入力された日付が正しいかどうかのバリデーションも重要です。

関連情報:

- https://www.geeksforgeeks.org/program-calculate-future-dates-using-timestamp/
- https://stackoverflow.com/questions/68537250/how-to-add-n-days-to-current-date-in-c
- https://www.tutorialspoint.com/c_standard_library/time_h.htm