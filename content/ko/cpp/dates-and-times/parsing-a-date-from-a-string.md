---
title:                "문자열에서 날짜 분석하기"
aliases: - /ko/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:01.280577-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 날짜 분석하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
문자열에서 날짜를 파싱하는 것은 문자열 형식을 해석하여 날짜 구성 요소인 날, 월, 연도를 추출하는 것을 포함합니다. 프로그래머들은 사용자 입력을 처리하거나, 데이터 파일을 읽거나, 날짜를 문자열 형식으로 통신하는 API와 상호 작용하기 위해 이 작업을 수행합니다. 이것은 데이터 처리, 유효성 검사 및 애플리케이션에서 날짜 산술을 수행하는 데 필수적입니다.

## 어떻게 하나요:
현대 C++에서는 `<chrono>` 라이브러리를 사용하여 날짜와 시간을 기본적으로 처리할 수 있지만, 복잡한 형식에 대한 수동 파싱 없이는 문자열에서 파싱을 직접 지원하지 않습니다. 그러나 ISO 8601 날짜 형식 및 간단한 사용자 정의 형식의 경우, 여기 파싱을 수행하는 방법이 있습니다.

**`<chrono>`와 `<sstream>` 사용하기:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 형식
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "파싱된 날짜: " << parsed_date << std::endl;
    } else {
        std::cout << "날짜 파싱 실패." << std::endl;
    }
    
    return 0;
}
```
샘플 출력:
```
파싱된 날짜: 2023-04-15
```

더 복잡한 형식을 다루거나 구버전 C++을 사용할 때는 `date.h` (Howard Hinnant의 날짜 라이브러리) 같은 타사 라이브러리를 자주 사용합니다. 다음은 이를 사용하여 다양한 형식을 파싱하는 방법입니다:

**`date.h` 라이브러리 사용하기:**
라이브러리가 설치되어 있는지 확인하세요. 여기서 찾을 수 있습니다 [여기](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "파싱된 날짜: " << parsed_date << std::endl;
    } else {
        std::cout << "문자열에서 날짜 파싱 실패." << std::endl;
    }

    return 0;
}
```
샘플 출력 (시스템의 로캘 및 날짜 설정에 따라 달라질 수 있음):
```
파싱된 날짜: 2023-04-15
```
