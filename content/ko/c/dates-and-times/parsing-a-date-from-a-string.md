---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:36.218332-07:00
description: "\uBC29\uBC95: C\uB294 \uBB38\uC790\uC5F4\uC5D0\uC11C \uC9C1\uC811 \uB0A0\
  \uC9DC\uB97C \uD30C\uC2F1\uD558\uB294 \uB0B4\uC7A5 \uBC29\uBC95\uC744 \uC81C\uACF5\
  \uD558\uC9C0 \uC54A\uC73C\uBBC0\uB85C, \uC885\uC885 POSIX \uC2DC\uC2A4\uD15C\uC6A9\
  \ `<time.h>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uC11C \uC0AC\uC6A9 \uAC00\uB2A5\
  \uD55C `strptime` \uD568\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774 \uD568\
  \uC218\uB294 \uC785\uB825 \uBB38\uC790\uC5F4\uC758 \uC608\uC0C1 \uD615\uC2DD\uC744\
  \ \uC9C0\uC815\uD558\uACE0 \uADF8\uAC83\uC744 \uAD6C\uC131 \uC694\uC18C\uB85C \uB098\
  \uB208 \uB2EC\uB825 \uB0A0\uC9DC \uBC0F\u2026"
lastmod: '2024-03-13T22:44:55.939999-06:00'
model: gpt-4-0125-preview
summary: "C\uB294 \uBB38\uC790\uC5F4\uC5D0\uC11C \uC9C1\uC811 \uB0A0\uC9DC\uB97C \uD30C\
  \uC2F1\uD558\uB294 \uB0B4\uC7A5 \uBC29\uBC95\uC744 \uC81C\uACF5\uD558\uC9C0 \uC54A\
  \uC73C\uBBC0\uB85C, \uC885\uC885 POSIX \uC2DC\uC2A4\uD15C\uC6A9 `<time.h>` \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uC5D0\uC11C \uC0AC\uC6A9 \uAC00\uB2A5\uD55C `strptime`\
  \ \uD568\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
C는 문자열에서 직접 날짜를 파싱하는 내장 방법을 제공하지 않으므로, 종종 POSIX 시스템용 `<time.h>` 라이브러리에서 사용 가능한 `strptime` 함수를 사용합니다. 이 함수는 입력 문자열의 예상 형식을 지정하고 그것을 구성 요소로 나눈 달력 날짜 및 시간을 나타내는 `struct tm`으로 파싱할 수 있게 합니다.

다음은 문자열에서 날짜를 파싱하기 위해 `strptime`을 사용하는 간단한 예제입니다:

```c
#include <time.h>
#include <stdio.h>

int main() {
    const char *dateStr = "2023-04-01";
    struct tm tm;
    char buf[255];

    // 문자열 날짜를 struct tm으로 파싱
    if (strptime(dateStr, "%Y-%m-%d", &tm) == NULL) {
        printf("날짜 파싱 실패.\n");
    } else {
        // strftime을 사용해 읽을 수 있는 형식으로 날짜를 출력
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("파싱된 날짜: %s\n", buf);
    }

    return 0;
}
```

이 프로그램의 예시 출력은 다음과 같습니다:

```
파싱된 날짜: 토요일, 4월 01일, 2023
```

패턴과 일치하지 않거나 예상치 못한 입력을 만났을 때와 같은 잠재적 오류를 처리하는 것이 중요합니다.

## 심층 분석
`strptime` 함수는 강력하지만, 표준 C 라이브러리에 포함되어 있지 않으며 주로 Linux와 UNIX와 같은 POSIX 호환 시스템에서 찾을 수 있습니다. 이 제한은 `strptime`을 사용하여 문자열에서 날짜를 파싱하는 프로그램이 추가 호환성 계층이나 라이브러리 없이는 Windows와 같은 비POSIX 시스템으로 이식할 수 없음을 의미합니다.

역사적으로, C에서 날짜와 시간을 처리하는 것은 다양한 로케일과 시간대를 고려할 때 많은 수작업 및 주의가 필요했습니다. C++의 `<chrono>` 라이브러리 및 Howard Hinnant의 C++용 날짜 라이브러리와 같은 현대 대안 및 확장은 포함하여 파싱하는 것을 포함하여 날짜와 시간 조작을 위한 보다 강력한 솔루션을 제공합니다. 이러한 라이브러리는 일반적으로 다양한 날짜 형식, 시간대 및 오류 처리 메커니즘에 대한 더 나은 지원을 제공하여 광범위한 날짜 및 시간 조작 기능이 필요한 새 프로젝트에 선호됩니다.

그럼에도 불구하고, C에서 문자열에서 날짜를 파싱하는 방법을 이해하는 것은 이러한 현대 도구를 사용할 수 없거나 엄격한 C 프로그래밍 환경의 제약 내에서 작업할 때 호환성을 유지해야 하는 프로젝트에서 작업하거나 유지 관리할 때 유용할 수 있습니다.
