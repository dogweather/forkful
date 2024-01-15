---
title:                "날짜를 문자열로 변환하는 것"
html_title:           "C++: 날짜를 문자열로 변환하는 것"
simple_title:         "날짜를 문자열로 변환하는 것"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 작업을 해야하는 이유는 코드에서 날짜를 보다 효율적으로 다루기 위해서입니다. 문자열 형태로 변환하면 보다 쉽게 다양한 날짜 형식을 사용할 수 있으며, 다른 데이터와 함께 처리하기도 용이합니다.

## 이렇게 하는 법

아래 코드 블록을 참고하여 날짜를 문자열로 변환하는 방법을 알아봅시다.

```C++
#include <iostream>
#include <iomanip>
#include <string>
#include <ctime>

int main() {
    // 현재 시간 가져오기
    std::time_t now = std::time(0);

    // tm 구조체 생성하기
    tm *mytime = std::localtime(&now);

    // 날짜 형식 지정하기
    char buffer[80];
    std::strftime(buffer, 80, "%Y-%m-%d %H:%M:%S", mytime);

    // 출력하기
    std::cout << "오늘의 날짜: " << buffer << std::endl;

    return 0;
}
```

출력 결과는 다음과 같이 나옵니다.

```
오늘의 날짜: 2021-05-04 15:30:00
```

## 더 깊이 들어가기

날짜를 문자열로 변환하는 작업은 시간과 날짜를 다루는 프로그램에서 매우 중요한 요소입니다. C++에서는 `std::time`과 `std::strftime` 함수를 사용하여 날짜와 시간을 다룰 수 있습니다. 또한 `std::tm` 구조체를 사용하여 날짜와 시간을 저장하고 조작할 수 있습니다. `strftime` 함수에서 사용할 수 있는 다양한 형식 지정자를 이용하면 원하는 날짜 형식을 손쉽게 만들 수 있기 때문에, 활용도가 매우 높습니다.

## 관련 정보

- [C++ Reference: std::time](https://en.cppreference.com/w/cpp/chrono/c/time)
- [C++ Reference: std::strftime](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [C++ Reference: std::tm](https://en.cppreference.com/w/cpp/chrono/c/tm)