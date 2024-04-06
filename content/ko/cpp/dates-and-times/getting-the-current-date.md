---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:16.912958-07:00
description: "\uC5B4\uB5BB\uAC8C: C++\uC740 `<chrono>` \uD45C\uC900 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uC640 Boost\uC640 \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uBE44\uB86F\uD55C \uC5EC\uB7EC \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD558\uC5EC \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uC744 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uB2E4\uC74C \uC608\uC81C\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uC218\
  \uD589\uD558\uB294 \uBC29\uBC95\uC744 \uBCF4\uC5EC\uC90D\uB2C8\uB2E4. C++20\uC740\
  \ `<chrono>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uB354 \uB9CE\uC740 \uAE30\uB2A5\
  \uC744\u2026"
lastmod: '2024-03-13T22:44:55.684438-06:00'
model: gpt-4-0125-preview
summary: "C++\uC740 `<chrono>` \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC640 Boost\uC640\
  \ \uAC19\uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uBE44\
  \uB86F\uD55C \uC5EC\uB7EC \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uC5EC \uD604\uC7AC\
  \ \uB0A0\uC9DC\uB97C \uC5BB\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 어떻게:
C++은 `<chrono>` 표준 라이브러리와 Boost와 같은 서드파티 라이브러리를 비롯한 여러 방법을 제공하여 현재 날짜를 얻을 수 있습니다. 다음 예제들은 이 작업을 수행하는 방법을 보여줍니다.

### `<chrono>` 사용하기 (C++20 이후)
C++20은 `<chrono>` 라이브러리에 더 많은 기능을 도입하여 현재 날짜를 얻는 것을 간단히 만들었습니다:
```cpp
#include <iostream>
#include <chrono>
#include <format> // std::format을 위해 (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // 현재 시간 캡처
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // time_t로 변환

    // 시간을 읽기 쉬운 형식으로 포맷
    std::cout << "현재 날짜: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**출력 예:**
```plaintext
현재 날짜: 2023-03-15
```

### `<ctime>` 사용하기
C++의 이전 버전을 사용하는 프로그래머들 또는 전통적인 C 라이브러리를 선호하는 이들을 위해:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // 현재 시간 얻기
    std::tm* now = std::localtime(&t);
    std::cout << "현재 날짜: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**출력 예:**
```plaintext
현재 날짜: 2023-03-15
```

### Boost Date_Time 사용하기
Boost 라이브러리를 사용하는 프로젝트의 경우, Boost Date_Time 라이브러리는 현재 날짜를 얻는 대체 방법을 제공합니다:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Boost의 그레고리안 캘린더를 사용해 현재 일자 얻기
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "현재 날짜: " << today << std::endl;

    return 0;
}
```
**출력 예:**
```plaintext
현재 날짜: 2023-Mar-15
```
이 예제들은 C++에서 날짜를 사용하여 작업하는 데 기본적인 기반을 제공하며, 다양한 애플리케이션에 필수적입니다.
