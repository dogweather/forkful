---
title:                "C: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

こんにちは、読者の皆さん。今回は、日付を文字列に変換する方法についてお話ししたいと思います。プログラミングで日付を扱う際には、変換する必要がある場合があります。では、なぜ日付を文字列に変換する必要があるのでしょうか？

## なぜ

プログラムで作業する際には、日付を文字列として表示したい場合がよくあります。例えば、日付をファイル名やデータベースのフィールド名に含める必要があるときや、ユーザーに日付をわかりやすく表示する必要があるときなどです。

## 手順

DateTime構造体を使って、日付を文字列に変換することができます。まずは```DateTime```構造体を宣言して、次のように初期化します。

```C
DateTime date = {year, month, day, hour, minute, second};
```

日付を文字列に変換するには、```strftime()```関数を使います。この関数は、指定したフォーマットに従って、日付を文字列に変換してくれます。例えば、日付をyyyy/mm/ddの形式に変換するには、次のようにコードを書きます。

```C
char str[20];
strftime(str, 20, "%Y/%m/%d", &date);
```

これで、変数```str```にyyyy/mm/ddの形式で日付が文字列として格納されます。もし、特定の言語における月や曜日の表記を使いたい場合は、```setlocale()```関数でその言語を指定してから```strftime()```関数を使うことで、その言語に合わせた形式で日付が表示されます。

## 深堀り

日付を文字列に変換する際に、よく使われるフォーマットにはいくつか種類があります。例えば、dd/mm/yyyyやyyyy-mm-ddなどがあります。また、```strftime()```関数では、年や月、日を数字ではなく文字列で表現することもできます。例えば、月をJanやFebruaryではなく、数字の1や2で表すこともできます。

日付を扱う際には、タイムゾーンの考慮やうるう年の判定などにも注意が必要です。また、さまざまなフォーマットに対応するためには、```strftime()```関数の扱い方を覚えておくことが重要です。

## 参考リンク

- [DateTime structure (C) - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
- [strftime() function (C) - Microsoft Docs](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/strftime-wcsftime-strftime-l-wcsftime-l?view=vs-2019)
- [C program to convert date into string - GeeksforGeeks](https://www.geeksforgeeks.org/program-to-convert-date-into-string/)

## 見てみる

ここでは、日付を文字列に変換するためのプログラムを実際に試してみることができます。実行結果を見ながら、どのようにプログラムを書けば良いのかを理解することができるかもしれません。

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 日付を入力
    int year = 2020;
    int month = 4;
    int day = 6;
    int