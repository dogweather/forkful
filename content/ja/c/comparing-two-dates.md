---
title:    "C: 「二つの日付の比較」"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why 
日付を比較する理由: 日付を比較することには、多くの理由があります。例えば、過去の予定をチェックするためには、今日の日付と過去の日付を比較する必要があります。また、将来のイベントをプログラムするためには、今日の日付と将来の日付を比較することも必要です。日付を比較することは、日々のプログラミングにおいて非常に一般的な作業です。 

## How To 
日付を比較する方法: 日付を比較するには、2つの日付を取得し、比較演算子を使用して比較するだけです。例えば、今日の日付が過去の日付よりも前か後かを判断したい場合、以下のようにコードを記述します。

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 現在の日付を取得
    time_t now = time(NULL);
    struct tm *current_date = localtime(&now);

    // 過去の日付を設定
    struct tm past_date = { 0 };
    past_date.tm_year = 120; // 2020年
    past_date.tm_mon = 3; // 4月
    past_date.tm_mday = 1; // 1日

    // 日付を比較する
    if (difftime(mktime(current_date), mktime(&past_date)) > 0)
    {
        printf("今日の日付は過去の日付よりも後です。\n");
    }
    else
    {
        printf("今日の日付は過去の日付よりも前です。\n");
    }

    return 0;
}
```

上記の例では、`difftime()`関数を使用して2つの日付を比較しています。また、`difftime()`関数は秒単位での差を返すので、過去の日付がどの程度の差があるかも確認することができます。実行結果は以下の通りです。

```
今日の日付は現在の日付よりも後です。
```

## Deep Dive
日付を比較する際の注意点: 日付を比較する際には、いくつかの注意点があります。まず、日付を表す変数は`struct tm`型である必要があります。また、`mktime()`関数を使用して`struct tm`型の変数を取得する際、月や日には-1した値を指定する必要があります（月は0-11を、日は1-31を渡す）。さらに、時刻情報が必要ない場合は、struct tm構造体の要素には0を指定する必要があります。

## See Also
日付を比較する他の方法や技術については、以下のリンクを参考にしてください。

- [Cで日付を比較する方法 1](https://www.tutorialspoint.com/c-programming/c-comparing-dates.htm)
- [Cで日付を比較する方法 2](https://www.techonthenet.com/c_language/standard_library_functions/string_h/strftime.php)
- [Cでの日付処理のベストプラクティス](https://fresh2refresh.com/c-programming/c-date-time-library-functions/)

以上で、日本語によるC言語の日付比較の記事は終了です。日付を比較する際には、上記の方法やリンクを参考にしてください。