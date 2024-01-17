---
title:                "「2つの日付を比較する」"
html_title:           "C: 「2つの日付を比較する」"
simple_title:         "「2つの日付を比較する」"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何をするために：
日付を比較することは、プログラマーがある日付が他の日付よりも前にあるか後ろにあるかを知るために行う作業です。これは、プログラマーが日付を使ってアプリケーションやゲームを作る際に欠かせないものです。

## 方法：
日付を比較するには、```C```コードブロック内で2つの日付を指定し、オペレーターを使用することで比較できます。例えば、以下のコードを使用することで、2つの日付の比較ができます。

```
if(date_1 > date_2){
    printf("日付1は日付2よりも未来です。\n");
}
else{
    printf("日付1は日付2よりも過去です。\n");
}
```

このコードでは、オペレーター```>```を使用して2つの日付を比較し、結果に応じて適切なメッセージを出力します。

## 深堀り：
日付を比較するための最も一般的な方法は、大きさ比較オペレーター```>```や```<```を使用することです。しかし、この方法では日付の精度が整数型に制限されてしまいます。そのため、日付を正確に比較するには、日付を表す構造体やクラスを使用したり、外部ライブラリを導入したりする必要があります。

また、日付を比較する場合には、タイムゾーンや夏時間などの情報も考慮する必要があります。これらの情報は、通常、UTC（協定世界時）を使用して比較することで解決できます。

## 関連情報：
日付を比較するにあたって、さらに詳細な情報や代替手段を知りたい方は、以下のサイトを参考にしてください。

- [日付を比較する方法（ウィキペディア）](https://ja.wikipedia.org/wiki/%E6%97%A5%E4%BB%98%E3%81%AE%E6%AF%94%E8%BC%83)
- [日付および時刻関数（C Standard Library）](https://www3.cs.stonybrook.edu/~cse220/spring14/slides/c80-dates-and-time-c-standard-library.pdf)
- [C++ Boost.Date_Timeライブラリ](https://theboostcpplibraries.com/boost.date-time)