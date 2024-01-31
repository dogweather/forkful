---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:35:59.817948-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환한다는 것은 시간 데이터를 읽기 쉬운 텍스트 형식으로 바꾸는 과정입니다. 프로그래머들이 이 작업을 수행하는 이유는 사용자에게 날짜 정보를 보이게 하거나, 날짜를 표준화된 형식으로 저장하기 위해서입니다.

## How to: (방법)
C에서 날짜를 문자열로 변환하기 위해 `strftime` 함수를 사용할 수 있습니다. 예를 들어보겠습니다.

```c
#include <stdio.h>
#include <time.h>

int main() {
    char str[100];
    time_t t = time(NULL);
    struct tm *timePtr = localtime(&t);

    strftime(str, sizeof(str), "%Y-%m-%d %H:%M:%S", timePtr);
    printf("Current DateTime is: %s\n", str);

    return 0;
}
```

```
Current DateTime is: 2023-04-05 14:20:30
```

이 코드는 현재 시간과 날짜 정보를 `YYYY-MM-DD HH:MM:SS` 형식으로 프린트합니다.

## Deep Dive (깊은 탐색)
`strftime` 함수는 C 표준 라이브러리의 일부로, 날짜와 시간을 다양한 형식으로 변환할 수 있도록 해줍니다. ANSI C가 선보인 이래로 많이 사용되어왔습니다.

C로 날짜를 문자열로 바꾸는 다른 방법도 있습니다. `sprintf` 함수를 사용하거나, C11 표준의 `timespec_get`과 함께 사용할 것입니다. `strftime`는 형식화된 출력에 힘을 주지만, 프로그래머가 형식을 직접 관리할 필요가 있는 경우 `sprintf`가 유용할 수 있습니다.

선택한 함수에 따라, 안전과 성능에서의 차이도 고려해야 합니다. 예를 들어, `strftime`은 형식 문자열을 잘못 사용했을 때의 실수로부터 보호받을 수 있는 반면, `sprintf`는 덜 안전할 수 있습니다.

## See Also (참조)
- C 표준 라이브러리 `strftime` 문서: https://en.cppreference.com/w/c/chrono/strftime
- GNU C 라이브러리 시간 관련 함수들: https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html
- C11 표준 `timespec_get` 사용법: https://en.cppreference.com/w/c/chrono/timespec_get
