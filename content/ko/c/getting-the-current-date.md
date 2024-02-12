---
title:                "현재 날짜 얻기"
aliases:
- ko/c/getting-the-current-date.md
date:                  2024-02-03T17:57:51.710127-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 얻기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

C에서 현재 날짜를 가져오는 것은 표준 C 라이브러리를 사용하여 시스템의 현재 날짜와 시간을 가져오고 형식화하는 것을 포함합니다. 프로그래머들은 자주 이 기능이 로깅, 시간 스탬핑 또는 애플리케이션 내의 일정 예약 기능을 위해 필요합니다.

## 방법:

C에서 `<time.h>` 헤더는 날짜와 시간을 다루기 위한 필요한 함수와 타입을 제공합니다. `time()` 함수는 현재 시간을 검색하고, `localtime()`은 이 시간을 현지 시간대로 변환합니다. 날짜를 표시하기 위해, 우리는 `strftime()`을 사용하여 문자열로 형식화합니다.

기본 예제는 다음과 같습니다:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // 현재 시간을 얻기
    time(&rawtime);
    // 현지 시간으로 변환
    timeinfo = localtime(&rawtime);
    
    // 날짜를 형식화하고 출력하기
    strftime(buffer, 80, "오늘의 날짜는 %Y-%m-%d입니다", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

샘플 출력은 다음과 같이 보일 수 있습니다:

```
오늘의 날짜는 2023-04-12입니다
```

## 심층 분석

`<time.h>`에 의해 용이해진 C에서의 시간 처리는 언어와 UNIX 시스템의 초기 날들로 거슬러 올라갑니다. 이것은 Unix Epoch(1970년 1월 1일) 이후 초로 표시된 현재 시간을 나타내는 `time_t` 데이터 유형을 중심으로 구축됩니다. 이것은 효율적이고 전 세계적으로 호환되는 반면, 이는 또한 표준 C 라이브러리의 시간 함수가 `time_t`의 범위와 해상도에 의해 본질적으로 한정된다는 것을 의미합니다.

고해상도 타임스탬프가 필요하거나 먼 미래 또는 과거의 날짜를 다루는 현대적 애플리케이션은 이러한 제한을 도전적으로 발견할 수 있습니다. 예로, 2038년 문제는 32비트 `time_t`를 사용하는 시스템이 오버플로될 것이라고 예시하는 유명한 일례입니다.

보다 복잡한 시간 및 날짜 처리를 위해, 많은 프로그래머들은 외부 라이브러리나 운영 시스템에 의해 제공되는 기능들로 전환합니다. 예를 들어, C++에서는 `<chrono>` 라이브러리가 보다 정밀하고 다양한 시간 조작 기능을 제공합니다.

그럼에도 불구하고, C의 시간 기능의 단순성과 보편성은 많은 애플리케이션에 완벽하게 적합합니다. 이 도구들을 이해하는 것은 C 프로그래머에게 기본이며, 역사적 프로그래밍 맥락과 실제적인 매일 유용성의 혼합을 제공합니다.
