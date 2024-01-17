---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "C: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

빠른날짜 계산을 위한 C 프로그래밍

## What & Why?

날짜 계산이란, 특정 날짜로부터 과거나 미래로 일정 시간을 계산하는 것을 말합니다. 프로그래머들은 주로 이것을 날짜/시간 기능을 사용하여 프로그램 내에서 일정한 작업을 수행할 때 필요합니다.

## How to:

날짜 계산을 위한 코드의 예시를 살펴보겠습니다. 우선 <time.h> 헤더 파일을 포함시키고, 두 날짜를 나타내는 구조체(예: struct tm)를 만들어야 합니다. 그 다음, 구조체의 값을 적절하게 설정하고, mktime() 함수를 사용하여 두 날짜 사이의 차이를 계산할 수 있습니다. 아래는 이 과정을 보여주는 예시 코드입니다.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm start_date = {0};
    start_date.tm_year = 2020 - 1900;
    start_date.tm_mon = 0;
    start_date.tm_mday = 1; // 2020년 1월 1일
    time_t start_time = mktime(&start_date);
    
    struct tm end_date = {0};
    end_date.tm_year = 2020 - 1900;
    end_date.tm_mon = 2;
    end_date.tm_mday = 15; // 2020년 3월 15일
    time_t end_time = mktime(&end_date);
    
    int difference = difftime(end_time, start_time) / (24 * 60 * 60); // 하루는 86,400초
    printf("두 날짜 사이의 차이는 %d일입니다.", difference);
    
    return 0;
}
```

위 코드에서는 2020년 1월 1일과 2020년 3월 15일 사이의 차이를 일단위로 출력합니다. 결과는 "두 날짜 사이의 차이는 74일입니다."가 될 것입니다.

## Deep Dive

날짜 계산은 컴퓨터 시스템에서 매우 중요한 역할을 합니다. 예를 들어, 파일의 만료일이나 휴가 기간 등과 같이 일정한 시간이 지나면 특정 작업이 필요한 경우에 자주 사용됩니다.

날짜 계산에는 여러 가지 방식이 존재하지만, 대표적으로 Julian Day와 UNIX Timestamp가 있습니다. Julian Day는 1582년 10월 15일 이전에 사용되는 달력 시스템을 바탕으로 하며, UNIX Timestamp는 1970년 1월 1일 이후부터 초 단위로 계산되는 시간을 나타냅니다.

## See Also

- [날짜와 시간 관련 함수 참조](https://ko.wikipedia.org/wiki/C_%EC%99%80_%ED%99%88%EC%8A%A4_%EC%8B%9C%EC%9E%91)
- [Julian Day에 대한 자세한 설명](https://ko.wikipedia.org/wiki/%EC%A3%BC%EB%A6%84%EB%8E%99%EC%8B%9C%EC%9D%98_%EA%B0%92)
- [UNIX Timestamp 정보](https://ko.wikipedia.org/wiki/UNIX_%EC%8B%9C%EA%B0%84)