---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:01.280577-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: \uD604\uB300 C++\uC5D0\uC11C\uB294\
  \ `<chrono>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB0A0\
  \uC9DC\uC640 \uC2DC\uAC04\uC744 \uAE30\uBCF8\uC801\uC73C\uB85C \uCC98\uB9AC\uD560\
  \ \uC218 \uC788\uC9C0\uB9CC, \uBCF5\uC7A1\uD55C \uD615\uC2DD\uC5D0 \uB300\uD55C\
  \ \uC218\uB3D9 \uD30C\uC2F1 \uC5C6\uC774\uB294 \uBB38\uC790\uC5F4\uC5D0\uC11C \uD30C\
  \uC2F1\uC744 \uC9C1\uC811 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\
  \uB7EC\uB098 ISO 8601 \uB0A0\uC9DC \uD615\uC2DD \uBC0F \uAC04\uB2E8\uD55C \uC0AC\
  \uC6A9\uC790 \uC815\uC758 \uD615\uC2DD\uC758 \uACBD\uC6B0, \uC5EC\uAE30\u2026"
lastmod: '2024-03-13T22:44:55.682770-06:00'
model: gpt-4-0125-preview
summary: "\uD604\uB300 C++\uC5D0\uC11C\uB294 `<chrono>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uAE30\uBCF8\
  \uC801\uC73C\uB85C \uCC98\uB9AC\uD560 \uC218 \uC788\uC9C0\uB9CC, \uBCF5\uC7A1\uD55C\
  \ \uD615\uC2DD\uC5D0 \uB300\uD55C \uC218\uB3D9 \uD30C\uC2F1 \uC5C6\uC774\uB294 \uBB38\
  \uC790\uC5F4\uC5D0\uC11C \uD30C\uC2F1\uC744 \uC9C1\uC811 \uC9C0\uC6D0\uD558\uC9C0\
  \ \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
