---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:35:08.433577-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付の解析とは、文字列から年、月、日を抽出することです。これは、ユーザー入力を取得したり、ファイルから読み込んだりした場合に日付データとして利用するために行います。

## How to (方法):
```C++
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string date_str = "2023-04-12";
    std::tm tm = {};
    std::istringstream ss(date_str);
    
    ss >> std::get_time(&tm, "%Y-%m-%d");
    
    if (ss.fail()) {
        std::cerr << "Date parsing failed." << std::endl;
        return 1;
    }

    // Outputting for demonstration purposes
    std::cout << "Year: " << tm.tm_year + 1900 << '\n';
    std::cout << "Month: " << tm.tm_mon + 1 << '\n';
    std::cout << "Day: " << tm.tm_mday << '\n';

    return 0;
}
```

Sample output:
```
Year: 2023
Month: 4
Day: 12
```

## Deep Dive (深層潜航):
Parsing a string to a date is a common task. Before C++11, developers often used `strptime` or custom parser functions. But these could be risky - they might not handle all date formats or could behave inconsistently across different platforms.

C++11 introduced `<chrono>` and `<iomanip>`, making date parsing safer and more standardized. The above example uses `<iomanip>`'s `get_time` function to ensure correct parsing according to the expected format. Alternatives are using third-party libraries like Boost.Date_Time or Howard Hinnant's date library.

Implementation wise, C++ now leverages types and standard library features to prevent common mistakes like out-of-range errors or month-day misinterpretations. Embrace these facilities for robust applications.

## See Also (関連リンク):
- [C++ reference for `<iomanip>`](https://en.cppreference.com/w/cpp/header/iomanip)
- [C++ reference for `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost.Date_Time documentation](https://www.boost.org/doc/libs/release/libs/date_time/)
- [Howard Hinnant's date library GitHub repository](https://github.com/HowardHinnant/date)