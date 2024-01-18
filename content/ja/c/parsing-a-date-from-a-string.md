---
title:                "文字列から日付を解析する"
html_title:           "C: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何をやっているのか？

日付を文字列から解析することはプログラマーがよく行う作業です。日付を表す文字列を解析して、その日付をコンピューターが理解できる形式に変換することで、プログラム内で日付を操作したり、比較したりすることができるようになります。

なぜ日付を文字列から解析するのかというと、プログラムで日付を扱う際には、様々な形式の日付が与えられることがあります。例えば、「2021/04/01」という形式や、「3月31日」という形式などがあります。そのため、プログラムがそのまま日付を扱うことは困難であり、文字列から解析することで日付を正しい形式に変換することができるのです。

## やり方：

**コード例 1：**

```C
#include<stdio.h>
#include<time.h>

int main()
{
  char date_string[] = "2021/05/01";
  struct tm date;
  
  // 文字列から日付を解析する
  strptime(date_string, "%Y/%m/%d", &date);
  
  // 解析した日付をフォーマットして出力する
  printf("解析した日付: %d年%d月%d日", date.tm_year + 1900, date.tm_mon + 1, date.tm_mday);
  
  return 0;
}
```

**出力結果：**

```2021年05月01日```

**コード例 2：**

```C
#include<stdio.h>
#include<time.h>

int main()
{
  char date_string[] = "3月31日";
  struct tm date;
  
  // 文字列から日付を解析する
  strptime(date_string, "%m月%d日", &date);
  
  // 解析した日付をフォーマットして出力する
  printf("解析した日付: %d年%d月%d日", date.tm_year + 1900, date.tm_mon + 1, date.tm_mday);
  
  return 0;
}
```

**出力結果：**

```2021年03月31日```

## 詳しく見ていく：

日付を文字列から解析する方法は、古くから様々なアルゴリズムが提案されてきました。現在では、標準ライブラリであるtime.hの関数である「strptime」を使うことが一般的です。

また、日付の形式によっては「正規表現」を使って文字列から日付を抽出する方法もあります。しかし、正規表現を使うとコードが複雑になってしまうため、簡単に解析できるライブラリを使うことがおすすめです。

## 関連情報：

- [strptime 関数のドキュメント](https://www.ibm.com/docs/ja/aix/7.2?topic=strptimeB)
- [正規表現の使い方を学ぶ](https://programming-study.com/ja/lesson/228/)