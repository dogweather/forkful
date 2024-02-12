---
title:                "날짜를 문자열로 변환하기"
aliases:
- /ko/c/converting-a-date-into-a-string.md
date:                  2024-02-03T17:54:14.597596-07:00
model:                 gpt-4-0125-preview
simple_title:         "날짜를 문자열로 변환하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

C에서 날짜를 문자열로 변환하는 것은 날짜 구조나 타임스탬프를 사람이 읽을 수 있는 형식으로 변환하는 것을 말합니다. 프로그래머들은 종종 로그, 사용자 인터페이스에 날짜를 표시하거나 JSON이나 CSV와 같은 텍스트 기반 형식으로 날짜를 저장할 때 이 작업을 수행합니다.

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
