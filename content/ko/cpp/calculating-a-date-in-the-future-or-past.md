---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "C++: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 뭐하고 왜하나요?
미래나 과거의 날짜를 계산하는 것은 일정 날짜로부터 특정 기간 후의 날짜를 찾아내는 프로그래밍 테크닉입니다. 프로그래머들이 이를 위해 사용하는 이유는, 예를 들자면, 예약 시스템에서 특정 시간 후의 예약을 처리하는 등의 상황에서 필요하기 때문입니다.

## 어떻게 하나요:
```C++
#include <iostream>
#include <ctime>
#include <chrono>

int main(){
    // 현재 시스템 시간 구하기 
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();

    // tm 형식으로 변환
    std::time_t tt = std::chrono::system_clock::to_time_t(now);
    std::tm* t= std::localtime(&tt);

    // 날짜 지정하기: 5일 후
    t->tm_mday+=5;

    // mktime로 시간 변환
    tt=std::mktime(t);
    
    // 출력 형식으로 변환하고 출력
    std::cout<<"Future date: "<<std::asctime(t)<<std::endl;

    return 0;
}
```
Output:
```C++
Future date: Mon Nov 1 22:07:57 2021
```
## 깊게 들어가보기:
이를 계산하는 방법은 오래 전부터 사용되어 왔으며, 이 문제에 관련된 많은 라이브러리와 기능이 있습니다. C++ 프로그래밍 언어에서는 `<ctime>` 및 `<chrono>` 라이브러리를 통해 다양한 시간과 날짜를 계산할 수 있습니다. 이 방법외에도, `boost::date_time` 라이브러리 등 거의 모든 날짜와 시간 요구사항을 보다 세밀하게 다루는 라이브러리가 존재합니다. 선택한 방법은 여러분의 용도와 개발에 필요한 세부사항에 따라 달라집니다.

## 참고하기:
참고 자료를 확인하면 더 많은 이해가 가능합니다:

[cplusplus.com - Chrono library](http://cplusplus.com/reference/chrono/): 기본적인 chrono 라이브러리 사용법을 배울 수 있습니다.

[cppreference.com - C library](https://en.cppreference.com/w/cpp/chrono/c): C++에서 C라이브러리를 사용하는 방법에 대한 자세한 정보를 얻을 수 있습니다.

[boost.org - Date Time library ](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html): 복잡한 시간과 날짜 계산이 필요하다면 Boost Date Time 라이브러리를 사용해 보는 것도 좋습니다.