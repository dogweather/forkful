---
title:                "현재 날짜 얻기"
html_title:           "C: 현재 날짜 얻기"
simple_title:         "현재 날짜 얻기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 현재 날짜 얻기:

## 무엇이고 왜 하는가?
현재 날짜를 얻는 것은 코드에서 현재 시간에 기준하여 작업을 처리하는 일반적인 방법입니다. 프로그래머들은 코드에서 날짜를 사용하여 로그를 기록하거나 시간 기반 작업을 계획하는 등 다양한 목적으로 날짜를 이용합니다.

## 방법:
```C
#include <stdio.h>
#include <time.h>

int main(){
    // 현재 날짜 구조체 생성
    struct tm *date;
    // 시간 정보 가져오기
    time_t now;
    time(&now);
    // 현재 날짜 구조체에 시간 정보 대입
    date = localtime(&now);
    // 날짜 정보 출력
    printf("%d년 %d월 %d일\n", date->tm_year + 1900, date->tm_mon + 1, date->tm_mday);

    return 0;
}
```

## 깊게 들어가보기:
(1) 현재 날짜를 얻는 코드는 운영체제가 제공하는 시간 함수를 사용하여 구현됩니다. 따라서 해당 운영체제의 문서를 참조하면 추가적인 정보를 얻을 수 있습니다.
(2) C언어에서는 localtime 함수를 사용하여 현재 시간을 구하는 것이 가장 일반적입니다. 하지만 이 외에도 다른 배열 형태로 정보를 저장할 수 있는 localtime_r 함수를 제공합니다.
(3) 날짜 정보를 다룰 때는 어떤 형식으로 출력할지 고민해야 할 필요가 있습니다. 이는 strftime 함수를 사용하여 원하는 형식으로 날짜를 출력할 수 있습니다.

## 참고하기:
- localtime 함수 문서: https://www.cplusplus.com/reference/ctime/localtime/
- strftime 함수 문서: https://www.cplusplus.com/reference/ctime/strftime/