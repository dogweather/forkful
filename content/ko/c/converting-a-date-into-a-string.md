---
title:                "날짜를 문자열로 변환하기"
html_title:           "C: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜?

날짜를 문자열로 변환하는 것에 대해 접근하는 이유는 다양합니다. 가장 일반적인 이유는 날짜 정보를 특정 형식으로 나타내기 위해서입니다. 예를 들어, YYYY-MM-DD 형식으로 날짜를 표시하거나 웹사이트에서 날짜를 다른 언어로 표시하는 등 다양한 용도로 사용됩니다.

## 어떻게 하나요?

날짜를 문자열로 변환하는 방법은 간단합니다. 전역 변수인 'tm' 구조체에서 년, 월, 일 등의 정보를 가져와서 원하는 문자열 형식에 맞게 조합해주면 됩니다. 다음은 C 코드로 작성된 예시와 출력 결과입니다.

```C
#include <stdio.h>
#include <time.h>

int main(){
    time_t now = time(NULL);
    struct tm *tm = localtime(&now);

    char date_string[20];

    // YYYY-MM-DD 형식으로 날짜 표시하기
    strftime(date_string, 20, "%Y-%m-%d", tm);
    printf("날짜: %s", date_string);

    return 0;
}
```

> 출력 결과:
>
> 날짜: 2021-05-27

## 깊숙한 곳으로

날짜를 문자열로 변환하는 더 깊은 내용을 살펴보겠습니다. C 에서는 시간을 나타내는 데에 'time_t' 라는 자료형을 사용하며, 이는 1970년 1월 1일 0시 0분 0초를 기준으로 한 초 단위로 측정됩니다. 또한 년, 월, 일 등의 정보를 가지고 있는 tm 구조체를 사용하여 원하는 형식으로 날짜를 조합할 수 있습니다.

See Also
- [C에 대한 기본적인 이해](https://ko.wikipedia.org/wiki/C_%EC%96%B8%EC%96%B4)
- [time 함수 사용법](https://modoocode.com/244)