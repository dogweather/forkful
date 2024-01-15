---
title:                "日付を文字列に変換する"
html_title:           "C: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングでは、日付を文字列に変換する必要があることがあります。例えば、ユーザーに対してわかりやすい形式で日付を表示するためや、データベースで日付を扱う際などです。

## 方法
「time.h」ヘッダーファイルを使用して、日付を文字列に変換することができます。まず、日付を表す構造体「tm」を宣言し、その中に年月日などの情報を入力します。その後、文字列として表示したい形式を指定して「strftime()」関数を使用します。以下のコードを参考にしてください。

```
#include <stdio.h>
#include <time.h>

int main()
{
    // 日付を表す構造体を宣言
    struct tm date;
    // 年月日の情報を入力
    date.tm_year = 2021 - 1900; // 年は1900からの経過年数で表す
    date.tm_mon = 4 - 1; // 4月
    date.tm_mday = 1; // 1日
    
    // 日付を文字列に変換して表示
    char date_str[11]; // 長さは11で十分
    strftime(date_str, 11, "%Y/%m/%d", &date); // YYYY/MM/DDの形式で変換
    printf("Converted date: %s", date_str); // 出力: Converted date: 2021/04/01
    
    return 0;
}
```

## ディープダイブ
「tm」構造体には、他にも様々な日付に関する情報が格納されています。例えば、曜日や時分秒などです。また、「strftime()」関数で指定できる形式も豊富で、英語や日本語などの言語設定や24時間制/12時間制なども選択できます。詳細な仕様は[公式ドキュメント](https://devdocs.io/c/time/strftime)を確認してください。

## 参考リンク
- [time.hヘッダーファイルについて](https://www.ibm.com/docs/ja/zos/2.4.0?topic=files-help-time_h)
- [公式ドキュメント：strftime()関数](https://devdocs.io/c/time/strftime)
- [struct tm構造体の説明](https://devdocs.io/c/time/tm)