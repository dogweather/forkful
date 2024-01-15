---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "C: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

과거나 미래의 날짜를 계산하는 것에 참여하는 이유는 일정을 관리하고 관리하는 것을 도와주기 때문입니다.

## 어떻게

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    struct tm now = { 0 }; //현재 날짜
    struct tm future = { 0 }; //미래 날짜
    struct tm past = { 0 }; //과거 날짜
    
    //현재 날짜 가져오기
    time_t t = time(NULL);
    now = *localtime(&t);
    
    //미래 날짜 계산하기
    future = now;
    future.tm_mday += 365; //365일 추가 (1년 후)
    mktime(&future); //tm structure를 현재 시스템의 시간에 맞게 변경해줌
    
    //과거 날짜 계산하기
    past = now;
    past.tm_mday -= 365; //365일 감소 (1년 전)
    mktime(&past); //tm structure를 현재 시스템의 시간에 맞게 변경해줌
    
    //결과 출력하기
    printf("현재 날짜: %d년 %d월 %d일\n", now.tm_year + 1900, now.tm_mon + 1, now.tm_mday);
    printf("미래 날짜: %d년 %d월 %d일\n", future.tm_year + 1900, future.tm_mon + 1, future.tm_mday);
    printf("과거 날짜: %d년 %d월 %d일\n", past.tm_year + 1900, past.tm_mon + 1, past.tm_mday);
    
    return 0;
}

/* 출력:
현재 날짜: 2021년 4월 5일
미래 날짜: 2022년 4월 5일
과거 날짜: 2020년 4월 5일
*/
```

## 기초 파헤치기

날짜를 계산하는 것은 여러 이유로 유용할 수 있습니다. 예를 들어, 일정한 날짜 간격을 유지하거나 특정 날짜에 이벤트를 예약하는 데 사용할 수 있습니다. 또한 C 언어에서 날짜를 계산하는 방법을 배우는 것은 프로그래밍 기초를 다지는 데 도움이 됩니다.

## 참고 자료

- [C 언어 랜덤 숫자 생성하기](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [C 언어 문자열 입출력 함수](https://www.geeksforgeeks.org/c-input-output/)
- [C 언어에서 날짜를 문자열로 바꾸기](https://stackoverflow.com/questions/2408958/how-to-get-a-date-in-c)
- [C 언어에서 문자열을 정수로 바꾸기](https://www.geeksforgeeks.org/c-program-convert-string-integer/)
- [C 언어로 미리 정의된 상수 사용하기](https://www.tutorialspoint.com/cprogramming/c_constants.htm)

## 봐도 좋아요

- [C 언어로 현재 시간 얻기](https://www.programmingunit.com/2012/10/15/c-tutorial-get-current-date-time/)
- [C 언어의 난수 생성기 함수](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [C 언어에서 여러 가지 날짜 형식 사용하기](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [C 언어에서 날짜와 시간을 표시하는 다양한 방법](https://www.guru99.com/c-date-time.html)