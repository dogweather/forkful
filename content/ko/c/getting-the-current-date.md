---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:13:27.306898-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
컴퓨터 프로그램에서 현재 날짜를 가져오기란 시스템 시간을 읽어와 사용자에게 보여주는 것이다. 이 기능은 이력 로깅, 경과 시간 측정 및 다양한 시간 관련 처리 작업에 필수적이다.

## How to: (방법:)
C에서 현재 날짜를 얻는 예제 코드는 다음과 같습니다.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    printf("오늘 날짜: %d-%02d-%02d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);

    return 0;
}
```

실행 결과:
```
오늘 날짜: 2023-04-15
```

## Deep Dive (심층 분석)
`time.h` 헤더 파일 안에는 날짜와 시간을 다루는 여러 함수와 타입이 있습니다. 우리는 `time()` 함수와 `struct tm` 구조체를 사용해 현재 날짜와 시간을 얻었습니다. `time()` 함수는 현재 시간을 시간대로부터 독립적인 `time_t` 타입으로 반환합니다. `localtime()` 함수는 이 `time_t` 값을 받아 사용자의 로컬 시간대에 맞춰 정보를 `struct tm`에 저장합니다.

C에서 날짜와 시간을 다루던 전통적인 방법이지만, 최근에는 더 강력하고 유연한 타입과 함수들을 제공하는 라이브러리, 예를 들어 POSIX `strftime()` 함수나 C++ `std::chrono` 라이브러리 등이 사용되기도 합니다.

시간을 정밀하게 측정하거나 국제 시간대를 다루기 위해서는 이보다 더 복잡한 처리가 필요합니다. 예를 들어, UNIX 타임스탬프는 시차 없는, 세계 표준시(UTC)를 기본으로 계산된 정수값으로 시간을 표현합니다.

## See Also (추가 자료)
- C 표준 라이브러리 설명: http://www.cplusplus.com/reference/ctime/
- POSIX `strftime()` 함수 사용법: https://man7.org/linux/man-pages/man3/strftime.3.html
- C++ `std::chrono` 라이브러리: https://en.cppreference.com/w/cpp/chrono
