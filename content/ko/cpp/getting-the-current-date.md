---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇, 왜요?

현재 날짜를 가져오기란, 컴퓨터의 시스템 클럭에서 현재 날짜를 읽는 것입니다. 프로그래머들은 파일들의 타임스탬프를 생성하거나, 시간의 흐름을 표현하기 위해 이를 많이 활용합니다.

## 어떻게?

다음은 C++에서 현재 날짜를 얻는 방법입니다.

```C++
#include <iostream>
#include <ctime>
 
int main()
{
    time_t now = time(0);
  
    tm *ltm = localtime(&now);
  
    std::cout << "Year: "<< 1900 + ltm->tm_year << std::endl;
    std::cout << "Month: "<< 1 + ltm->tm_mon << std::endl;
    std::cout << "Day: "<< ltm->tm_mday << std::endl;

    return 0;
}
```

이 프로그램을 실행하면 현재 날짜가 출력됩니다. 예를 들어,

```C++
Year: 2022
Month: 10
Day: 16
```

## 깊게 알아보기

현재 날짜 가져오기는 매우 오래전부터 있었으며, 오늘날 많은 프로그래밍 언어들이 이를 기본 라이브러리로 제공합니다.

여러 방법이 있습니다만, 가장 간단하고 흔하게 사용하는 방법이 `ctime` 라이브러리를 이용하는 것입니다. 이 라이브러리는 C 언어에서 물려받은 것이기 때문에 기본적으로 사용되기도 합니다.

하지만, 만약 더 높은 정확도가 필요하거나, 시간대를 다루고 싶다면, C++20의 `chrono` 라이브러리를 확인해보세요. 이 라이브러리는 복잡하지만, 매우 강력하고 세밀하게 시간을 다룰 수 있습니다.

## 참고하기

더 많은 정보를 얻기 위해선 다음 링크들을 참조하세요:

[chrono library](https://en.cppreference.com/w/cpp/chrono)

[how to get current time and date in C++](https://www.geeksforgeeks.org/how-to-get-current-time-and-date-in-c/)

[Time, Date and Time zones in Modern C++](https://akrzemi1.wordpress.com/2011/05/13/dates-and-times-in-c/)