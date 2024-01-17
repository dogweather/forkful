---
title:                "現在の日付を取得する"
html_title:           "C: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何かをするってどういうこと？ 
現在の日付を取得することは、プログラマーにとって非常によく行われるタスクです。コンピューター上で操作する日付や時間を特定するために使用されることがあります。 

## 方法： 
C言語で現在の日付を取得する方法は非常にシンプルです。以下のコードを使用することができます。

```
#include <stdio.h> 
#include <time.h> 

int main()
{
   // 現在の日付を取得する 
   time_t now;
   time(&now);

   // 日付をフォーマットする 
   char* dateTime = ctime(&now);

   // 結果を出力する 
   printf("現在の日付は: %s\n", dateTime);
    
   return 0;
}
```

出力は以下のようになります。

```
現在の日付は: Mon Oct 11 15:37:00 2021
```

## 深く潜る： 
現在の日付を取得する方法は、C言語で使用可能な様々な関数を使って行うことができます。代表的なものには、`time()`関数や`localtime()`関数があります。また、コンピューター内部で使われている「エポック秒」というものがあり、1970年1月1日からの経過秒数を表すものです。

## 関連リンク： 
- [`time()`関数のドキュメンテーション](https://www.c-tutorial-language.com/time-time_t/)
- [`localtime()`関数のドキュメンテーション](https://www.c-tutorial-language.com/localtime-struct-tm/)