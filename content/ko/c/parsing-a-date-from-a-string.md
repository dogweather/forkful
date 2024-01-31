---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:34:57.621979-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

문자열에서 날짜를 파싱하는 것은 문자열 형식의 날짜를 컴퓨터가 이해할 수 있는 형태로 변환하는 과정입니다. 이러한 변환이 필요한 이유는, 데이터 처리나 유효성 검사, 정렬과 같은 작업을 수행하기 위해서입니다.

## How to: (방법)

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *input_str = "2023-03-15";
    strptime(input_str, "%Y-%m-%d", &tm);

    printf("Year: %d, Month: %d, Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    // Sample Output: Year: 2023, Month: 3, Day: 15

    return 0;
}
```

## Deep Dive (깊이 알아보기)

날짜를 문자열에서 파싱하는 일은 프로그래밍 초기부터 있어왔습니다. 이전 방식 중 하나는 `sscanf`를 사용하는 것이었지만, 새로운 표준은 `strptime` 함수가 제공되며, 이것이 POSIX 표준에 부합합니다. `strptime` 함수는 문자열의 포맷을 지정하여 날짜와 시간을 파싱할 수 있게 해줍니다. C11이나 그 이후 버전을 사용하는 환경에서는 `get_time`이라는 더 새로운 대안이 있을 수 있습니다. 이 구현은 시스템과 로케일에 따라 다를 수 있으며, 다양한 날짜 포맷을 지원하기 위해 format specifiers를 사용합니다.

## See Also (더 보기)

- C 표준 라이브러리 문서: https://en.cppreference.com/w/c/chrono/strptime
- POSIX 표준: https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html
- `strftime`와 `strptime` 포맷 지정자 가이드: http://man7.org/linux/man-pages/man3/strftime.3.html
