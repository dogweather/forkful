---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:13:39.773282-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜 가져오기는 컴퓨터의 시스템 시간을 사용하여 현재 날짜를 확인하는 과정입니다. 이 기능은 기록, 이벤트 로깅, 유저 트래킹과 같이 타임스탬프가 필요한 상황에서 중요합니다.

## How to: (실행 방법)
C++20 라이브러리는 `<chrono>`를 사용하여 날짜와 시간을 쉽게 다룰 수 있습니다. 아래 예시 코드를 보고 따라 해 보세요.

```C++
#include <iostream>
#include <chrono>
#include <format>

int main() {
    using namespace std::chrono;
    auto current_time = system_clock::now(); // 현재 시간 가져오기
    time_t time = system_clock::to_time_t(current_time);
    auto local_time = *std::localtime(&time);

    std::cout << "Current Date (YYYY-MM-DD): ";
    std::cout << std::format("{:0>4}-{:0>2}-{:0>2}\n", local_time.tm_year + 1900, local_time.tm_mon + 1, local_time.tm_mday);

    return 0;
}
```
Sample Output:
```
Current Date (YYYY-MM-DD): 2023-04-15
```
이 코드는 현재 시스템 시간을 년-월-일 형식으로 표시합니다.

## Deep Dive (심층 분석)
C++에서 현재 날짜를 얻는 여러 방법 중 `<chrono>` 라이브러리가 가장 최신이며 다루기 쉽습니다. C++11 버전 이후로 많은 개선이 이루어져서, 복잡한 날짜 계산에서 사용할 수 있는 풍부한 기능을 제공합니다. 과거에는 `<ctime>`이 주로 사용되었지만, `<chrono>`가 표준으로 자리잡음에 따라 더 안전하고 직관적인 API를 사용할 수 있게 되었습니다. 대안으로는 각 운영 체제마다 제공하는 네이티브 API를 직접 호출하는 방법이 있지만, 이 방법은 이식성이 떨어질 수 있습니다. `<chrono>`는 타입 안전성과 함께 시간 존(zone)을 고려한 처리도 가능하게 합니다, 무엇보다도 표준 라이브러리의 일부이기 때문에 확장성과 호환성 면에서 추천합니다.

## See Also (참고 자료)
- [cppreference.com's chrono library page](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com's ctime library page](https://en.cppreference.com/w/cpp/header/ctime)
- [The ISO C++ site for learning about the latest features](https://isocpp.org/)

기본적인 내용을 다뤘지만, 날짜와 시간을 다루는 것에는 더 많은 면이 있습니다. 위 링크에서 좀 더 깊이 있는 정보를 찾을 수 있습니다.