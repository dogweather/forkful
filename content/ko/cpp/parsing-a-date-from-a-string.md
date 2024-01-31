---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:35:19.311293-07:00
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
문자열에서 날짜 파싱은 문자열 형태로 저장된 날짜 정보를 읽어와서 프로그램이 이해할 수 있는 날짜 타입으로 변환하는 과정입니다. 프로그래머들은 사용자가 입력한 날짜 데이터를 처리하거나 날짜 형식을 변경할 필요가 있을 때 이 작업을 수행합니다.

## How to (방법)
```C++
#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include <ctime>

int main() {
    std::string dateStr = "2023-04-05";
    std::tm tm = {};
    std::istringstream ss(dateStr);
    
    ss >> std::get_time(&tm, "%Y-%m-%d");
    
    if(ss.fail()) {
        std::cerr << "Date format error!" << std::endl;
        return 1;
    }
    
    // Use parsed date here - just printing for example
    std::cout << "Year: " << 1900 + tm.tm_year << ", Month: " 
              << 1 + tm.tm_mon << ", Day: " << tm.tm_mday << std::endl;

    return 0;
}
```
Sample output:
```
Year: 2023, Month: 4, Day: 5
```

## Deep Dive (심층 분석)
Parsing dates has been a common problem since the early days of programming, often handled by standard libraries or custom functions. C++ historically relied on the C `strptime` function, but this was not standardized in Windows. C++11 introduced `<chrono>` along with improved date/time facilities, and C++20 brought even more with `<chrono>`'s `std::chrono::year_month_day`, offering a more robust solution. The modern alternatives include third-party libraries like Boost.DateTime before C++11, and even `std::get_time` relies on the time.h facilities under the hood. Implementation details often revolve around handling various date formats and locales, an important aspect considering globalization of software.

## See Also (더 보기)
- C++ `std::get_time` - https://en.cppreference.com/w/cpp/io/manip/get_time
- C++ date and time utilities - https://en.cppreference.com/w/cpp/chrono
- Boost.DateTime library - https://www.boost.org/doc/libs/release/libs/date_time/
- Internationalization and localization considerations - https://www.gnu.org/software/libc/manual/html_node/Locales.html
