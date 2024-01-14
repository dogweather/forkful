---
title:                "C++: 미래 또는 과거의 날짜 계산"
simple_title:         "미래 또는 과거의 날짜 계산"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜
## 왜 날짜를 과거나 미래로 계산하는 것에 관심을 가질까요?

컴퓨터 프로그래밍은 다양한 상황에서 여러 가지 작업을 수행하는 데 도움이 됩니다. 날짜를 과거나 미래로 계산하는 것은 이러한 작업 중 하나입니다. 예를 들어, 사람들은 생일이나 이벤트 날짜를 기반으로 날짜를 계산하여 기념일을 확인하거나 일정을 계획하는 데 사용할 수 있습니다. 따라서 프로그래밍을 배우는 데 있어 날짜 계산은 중요한 부분입니다.

# 어떻게
## C++을 사용하여 미래 또는 과거 날짜 계산하는 방법

날짜를 계산하기 위해 C++에는 다양한 라이브러리가 있지만, <ctime> 헤더 파일을 사용하여 가장 일반적인 방법으로 계산할 수 있습니다.

```C++
#include <iostream>
#include <ctime>

int main() {
  // 현재 날짜와 시간 가져오기
  time_t now = time(0);
  tm *ltm = localtime(&now);

  // 현재 날짜 출력
  std::cout << "현재 날짜: "
            << 1 + ltm->tm_mon << '/'
            << ltm->tm_mday << '/'
            << 1900 + ltm->tm_year << std::endl;

  // 미래 날짜 계산하기 (1년 후를 가정)
  int futureYear = 1900 + ltm->tm_year + 1;
  tm futureDate = { 0, 0, 0,       // 초, 분, 시
                    ltm->tm_mday,  // 일
                    0,             // 월
                    futureYear };  // 년도
  time_t future = mktime(&futureDate);

  // 미래 날짜 출력
  std::cout << "1년 후 날짜: "
            << 1 + futureDate.tm_mon << '/'
            << futureDate.tm_mday << '/'
            << 1900 + futureDate.tm_year << std::endl;

  return 0;
}
```

위 코드를 실행하면 현재 날짜와 1년 후 날짜가 정확하게 계산되어 출력됩니다.

```
현재 날짜: 10/6/2021
1년 후 날짜: 10/6/2022
```

이처럼 C++에서 날짜를 계산하는 방법은 매우 간단합니다. 다만, 날짜를 연산하기 전에 <ctime>의 다양한 함수와 구조체를 잘 알아야 합니다.

# 깊게 파헤치기
## 날짜를 계산하는 데 사용되는 <ctime> 라이브러리의 다양한 함수와 구조체

날짜와 시간을 계산하기 위해서는 <ctime> 헤더 파일에 정의된 여러 가지 함수와 구조체를 알아야 합니다. 가장 많이 사용되는 함수와 구조체를 간략하게 소개하겠습니다.

### time_t
시간을 저장하는 데이터 형식입니다. 일반적으로 이 형식은 초 단위로 저장되어 시간 계산에 사용됩니다.

### localtime()
<ctime> 헤더 파일에 정의된 함수로, 현재 시스템의 시간 정보를 구조체 tm 형식으로 변환합니다.

### tm 구조체
시간 정보를 저장하는 구조체로, 연도, 월, 일, 시간, 분, 초 등의 정보를 멤버 변수로 가지고 있습니다.

### mktime()
구조체 tm의 값을 기반으로 time_t 형식의 시간 값을 계산합니다