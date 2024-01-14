---
title:                "C++: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換するには、プログラマーが日付を扱うために必要不可欠なタスクです。たとえば、特定の日付のフォーマットを変更したり、データベースに日付を格納するために文字列に変換する必要があるかもしれません。このブログポストでは、日付を文字列に変換する方法を学ぶことで、より柔軟に日付を扱うことができるようになります。

## 方法

C++では、dateをstringに変換する方法がいくつかあります。それぞれの方法を以下のコードブロックで示します。

```C++
// 標準的な文字列フォーマット
time_t now = time(0);
tm* local_time = localtime(&now);
char str_buffer [80];
strftime(str_buffer,80,"%Y-%m-%d",local_time);
std::string result1 = string(str_buffer);

// 自分でフォーマットを指定する場合
char buffer [80];
sprintf (buffer, "%d/%d/%d", local_time->tm_mon+1, local_time->tm_mday, local_time->tm_year+1900);
std::string result2 = string(buffer);
```

上記のコードでは、現在の日付を指定したフォーマットで文字列に変換しています。`result1`では、`strftime`関数を使用し、年-月-日の形式で日付を表しています。一方、`result2`では、自分でフォーマットを指定しています。このように、標準的なフォーマットや独自のフォーマットを指定することができます。

## ディープダイブ

日付を文字列に変換する際には、タイムゾーンやロケールに注意する必要があります。`localtime`関数を使用する際には、システムのデフォルトのタイムゾーンやロケールが使用されるため、意図しない結果になる可能性があります。そのため、必要に応じて`setlocale`関数や`tzset`関数を使用することでタイムゾーンやロケールを指定することができます。

また、C++11からは、`to_string`メソッドを使用することで、日付を文字列に変換することもできます。このメソッドを使用すると、簡単に日付を文字列に変換することができます。

## それでは

- [C++ date library](https://github.com/HowardHinnant/date)
- [strftime reference](http://www.cplusplus.com/reference/ctime/strftime/)
- [setlocale reference](http://www.cplusplus.com/reference/clocale/setlocale/)
- [tzset reference](http://www.cplusplus.com/reference/ctime/tzset/)
- [to_string reference](http://www.cplusplus.com/reference/string/to_string/)