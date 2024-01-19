---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 및 왜?
문자열에서 날짜를 구문 분석하는 것은 문자열로 표현된 날짜 정보를 프로그램이 이해하고 사용할 수 있는 형식으로 변환하는 과정입니다. 이는 사용자가 입력한 날짜 정보를 프로그램에서 사용하고자 할 때 주로 사용됩니다.

## 사용 방법:
C 언어에서 `strptime` 함수를 사용하여 문자열에서 날짜를 분석하려면 다음과 같은 코드를 작성할 수 있습니다. 
```C
#include <time.h>
#include <stdio.h>

int main()
{
    struct tm tm;
    char buf[255];

    strptime("2022-09-14 23:50", "%Y-%m-%d %H:%M", &tm);
    strftime(buf, sizeof(buf), "%d %b %Y %H:%M", &tm);

    printf("Parsed date is: %s\n", buf);

    return 0;
}
```
콘솔에서 위 프로그램을 실행하면 다음과 같은 결과를 볼 수 있습니다.
```
Parsed date is: 14 Sep 2022 23:50
```

## 심층 탐구:
날짜 구문 분석은 사용자 인터페이스 및 시간 관련 작업에서 필수적인 프로그래밍 작업입니다. 일반적으로 문자열로 표현된 날짜 형식은 사용자나 시스템이 입력한 것이며, 프로그램은 이 정보를 해석하여 사용해야 합니다.

C 언어에서는 `strptime` 함수를 사용하여 문자열에서 날짜 정보를 해석할 수 있습니다. 하지만 언어마다 사용하는 날짜 구문 분석 함수가 다를 수 있으며, 때로는 구체적인 요구에 따라 직접 구문 분석 코드를 작성해야 할 수도 있습니다.

## 참조 자료:
1. [C library function - Strptime](https://www.programiz.com/c-programming/library-function/time.h/strptime)
2. [C library function - Strftime](https://www.programiz.com/c-programming/library-function/time.h/strftime)
3. [ISO 8601 date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)