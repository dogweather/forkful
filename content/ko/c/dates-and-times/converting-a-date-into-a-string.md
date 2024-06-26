---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:14.597596-07:00
description: "\uBC29\uBC95: \uC774 \uBAA9\uC801\uC744 \uC704\uD574 `<time.h>` \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uC758 `strftime` \uD568\uC218\uAC00 \uC77C\uBC18\uC801\uC73C\
  \uB85C \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uC774 \uD568\uC218\uB294 \uD615\uC2DD \uC9C0\
  \uC815\uC790\uB97C \uC9C0\uC815\uD568\uC73C\uB85C\uC368 \uB2E4\uC591\uD55C \uBC29\
  \uC2DD\uC73C\uB85C \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uD615\uC2DD\uD654\uD560\
  \ \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uC5EC\uAE30 \uAC04\uB2E8\uD55C \uC608\
  \uC2DC\uAC00 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.943253-06:00'
model: gpt-4-0125-preview
summary: "\uC774 \uBAA9\uC801\uC744 \uC704\uD574 `<time.h>` \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC758 `strftime` \uD568\uC218\uAC00 \uC77C\uBC18\uC801\uC73C\uB85C \uC0AC\
  \uC6A9\uB429\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## 방법:
이 목적을 위해 `<time.h>` 라이브러리의 `strftime` 함수가 일반적으로 사용됩니다. 이 함수는 형식 지정자를 지정함으로써 다양한 방식으로 날짜와 시간을 형식화할 수 있게 합니다. 여기 간단한 예시가 있습니다:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // 날짜 & 시간을 문자열로 변환합니다 (예: "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("현재 날짜와 시간: %s\n", dateStr);
    return 0;
}
```

샘플 출력은 다음과 같을 수 있습니다:

```
현재 날짜와 시간: Wed Jun 30 21:49:08 2021
```

`strftime`에 전달된 형식 지정자를 변경하여 형식을 사용자 정의할 수 있습니다. 예를 들어, 날짜를 `YYYY-MM-DD` 형식으로 얻고 싶다면 `"%Y-%m-%d"`을 사용하면 됩니다.

## 심화 학습
`strftime` 함수와 `<time.h>` 라이브러리는 원래 ANSI C 표준(C89/C90)으로 거슬러 올라가는 C 표준 라이브러리의 일부입니다. 많은 플랫폼에서 지원되고 직관적인 날짜와 시간 라이브러리를 제공하는 현대적인 프로그래밍 언어에 비해 저수준이고 번거로운 것처럼 보일 수 있지만, 간단하고 폭넓게 지원됩니다.

C 표준 라이브러리의 시간 함수들은 널리 지원되고 사용하기에 상대적으로 간단하지만, 새로운 언어의 라이브러리 혹은 International Components for Unicode (ICU)와 같은 제3자 C 라이브러리에서 찾아볼 수 있는 보다 복잡한 시간대 조작 및 국제화 기능이 부족합니다.

그러나, `strftime` 함수의 커스터마이징 기능과 넓은 플랫폼 지원은 C에서 날짜 문자열 변환을 위한 신뢰할 수 있고 유용한 도구로 만듭니다. 더 높은 수준의 datetime 라이브러리를 갖춘 언어에서 오는 프로그래머들은 이것의 저수준 특성에 적응해야 할 수도 있지만, 다양한 응용 프로그램을 위해 날짜와 시간을 형식화하는 데 있어 놀라울 정도로 강력하고 다재다능함을 발견하게 될 것입니다.
