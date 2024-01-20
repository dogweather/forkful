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

## 무엇이고 왜?

'현재 날짜 얻기'는 컴퓨터 시스테ム의 흐른 시간을 판별하기 위한 일반적인 작업입니다. 이것은 로깅, 타임스탬프, 레포트 생성 등 다양한 애플리케이션에서 중요한 역할을 합니다.

## 어떻게:

```C
#include <time.h>
#include <stdio.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    printf("현재 날짜와 시간: %d-%02d-%02d %02d:%02d:%02d\n",
           tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
           tm.tm_hour, tm.tm_min, tm.tm_sec);
    return 0;
}
```

이 코드를 실행하면, 출력은 다음과 같습니다:

```C
현재 날짜와 시간: 2021-08-27 14:30:00
```

## 딥 다이브

1. 역사적 맥락: 초기의 컴퓨터 시스템에서는 '시간'을 추적하지 않았지만, 컴퓨팅이 발전함에 따라 시간 정보는 점점 중요해졌습니다.
2. 대체 방법: C 언어 외에도 Python, Java 등 다른 언어들도 현재 시간을 얻어오는 함수를 제공합니다. 
3. 구현 세부사항: `time_t`과 `struct tm`은 시간을 다루는 C 언어의 주요 구조입니다. `time(NULL)`은 현재 시간을 초 단위로 반환하고, `localtime(&t)`은 초 단위의 시간을 년, 월, 일 등의 구조로 분리합니다.

## 참고 링크

- "time.h" 라이브러리에 대한 자세한 사항: [https://www.cplusplus.com/reference/ctime/](https://www.cplusplus.com/reference/ctime/)