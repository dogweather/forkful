---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 불러오는 작업에 참여하는 이유는 다양합니다. 가장 일반적인 이유는 프로그램의 로그에 날짜와 시간을 기록하는 것입니다. 또한 특정한 기간에만 작동하는 기능을 추가하는 등 다양한 용도로 날짜를 사용할 수 있습니다.

## 어떻게

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 현재 시간 구조체 선언
    struct tm *st;
    // 시간 정보를 담을 char 배열 선언
    char date[20];
    // time 함수를 사용해 현재 시간 가져오기
    time_t now = time(0);
    // localtime 함수를 이용해 현재 시간 정보를 st 구조체에 저장
    st = localtime(&now);
    // strftime 함수를 이용해 날짜 형식 지정
    strftime(date, 20, "%Y-%m-%d", st);
    // 날짜 출력
    printf("현재 날짜: %s\n", date);
    
    return 0;
}
```

위 코드는 time.h 헤더 파일을 사용해 현재 시간을 가져오고, localtime 함수를 이용해 날짜 정보를 구조체에 저장하는 방법을 보여줍니다. 그리고 strftime 함수로 날짜 형식을 지정하고 출력하는 예제입니다.

출력 결과:

```
현재 날짜: 2020-07-29
```

## 깊이 파고들기

날짜를 가져오는 작업은 운영 체제로부터 시스템 시간 정보를 가져오는 것이기 때문에, 프로그래밍 언어마다 조금씩 다른 구현 방식이 있을 수 있습니다. C 언어의 경우 time.h 헤더 파일에서 제공하는 여러 함수를 이용해 현재 시간 정보를 가져오고, strftime 함수를 이용해 날짜 형식을 지정할 수 있습니다.

## 더 알아보기

* [C Programming: Date and Time functions](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
* [C 함수 레퍼런스 - time, localtime, strftime](https://modoocode.com/84)
* [시스템 시간 정보 얻어오기 (Linux)](https://ansohxxn.github.io/unix/linux-get-system-date-time/)