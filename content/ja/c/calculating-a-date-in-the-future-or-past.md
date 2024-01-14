---
title:                "C: 「未来や過去の日付の計算」"
simple_title:         "「未来や過去の日付の計算」"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
日付を未来や過去に計算する理由は、特定の日付に関する特定の情報を知りたいからです。例えば、誕生日から何日後の日付かを知りたいときなどが挙げられます。

## 方法
基本的な日付の計算には、標準ライブラリで提供される構造体「tm」を使用します。以下のコード例を参考にしてください。

```C
#include <time.h>

int main()
{
  // 現在の日付を取得
  time_t current = time(NULL);
  
  // 構造体tmに変換
  struct tm *current_date = localtime(&current);
  
  // 年月日を設定
  // 例：次の誕生日は今年の10月1日の場合
  current_date->tm_year = current_date->tm_year;
  current_date->tm_mon = 9;
  current_date->tm_mday = 1;
  
  // tm構造体から日付を計算
  // 例：次の誕生日までの日数を計算
  time_t future = mktime(current_date);
  int days = (future - current)/(60*60*24);
  
  // 結果を出力
  printf("次の誕生日まであと %d 日\n", days);
  
  return 0;
}
```

このコードを実行すると、現在の日付から次の誕生日までの日数を計算できます。

## 深堀り
日付を計算する際には、夏時間や閏年などの概念も考慮する必要があります。そうした特殊な条件を扱う場合には、外部のライブラリを使用することもできます。また、UNIX系のシステムでは、エポック秒を使用する方法やPerlのモジュールを使用する方法など、様々な手段があります。

## 他の記事を見る
- [C言語の標準ライブラリマニュアル](https://ja.wikipedia.org/wiki/C%E8%A8%80%E8%AA%9E%E3%81%AE%E6%A8%99%E6%BA%96%E3%83%A9%E3%82%A4%E3%83%96%E3%83%A9%E3%83%AA)
- [UNIX時間](https://ja.wikipedia.org/wiki/UNIX%E6%97%A5%E6%9C%AC)
- [Date::Calc - Perlのモジュール](https://metacpan.org/pod/Date::Calc)