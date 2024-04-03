---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:51.710127-07:00
description: "\uBC29\uBC95: C\uC5D0\uC11C `<time.h>` \uD5E4\uB354\uB294 \uB0A0\uC9DC\
  \uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C \uD544\uC694\uD55C \uD568\
  \uC218\uC640 \uD0C0\uC785\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. `time()` \uD568\uC218\
  \uB294 \uD604\uC7AC \uC2DC\uAC04\uC744 \uAC80\uC0C9\uD558\uACE0, `localtime()`\uC740\
  \ \uC774 \uC2DC\uAC04\uC744 \uD604\uC9C0 \uC2DC\uAC04\uB300\uB85C \uBCC0\uD658\uD569\
  \uB2C8\uB2E4. \uB0A0\uC9DC\uB97C \uD45C\uC2DC\uD558\uAE30 \uC704\uD574, \uC6B0\uB9AC\
  \uB294 `strftime()`\uC744 \uC0AC\uC6A9\uD558\uC5EC\u2026"
lastmod: '2024-03-13T22:44:55.941609-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C `<time.h>` \uD5E4\uB354\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\
  \uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C \uD544\uC694\uD55C \uD568\uC218\uC640 \uD0C0\
  \uC785\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uC5BB\uAE30"
weight: 29
---

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
