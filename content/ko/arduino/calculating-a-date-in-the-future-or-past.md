---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Arduino: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

감서들이 날짜를 미래나 과거로 계산하는 것에 관심을 갖는 이유는 프로젝트에 필요한 정확한 시간을 설정하기 위해서입니다.

## 왜 계산하는가?

날짜는 우리 일상생활에서 매우 중요한 역할을 합니다. 프로젝트를 진행하거나 일정을 관리할 때 정확한 날짜와 시간을 알아야만 합니다. 하지만 때로는 미래나 과거의 특정한 날짜를 계산해야 할 때가 있습니다. 이를 위해 아두이노에서 날짜를 계산하는 방법을 알아보겠습니다.

## 방법

아두이노에서 날짜를 계산하는 가장 간단한 방법은 `day()` 함수를 사용하는 것입니다. 이 함수는 현재 날짜를 숫자로 반환해줍니다. 예를 들어, 오늘이 2021년 12월 1일일 경우, `day()` 함수는 1을 반환합니다.

```Arduino
int day = day(); // 현재일을 day 변수에 저장
Serial.println(day); // 시리얼 모니터에 day 값 출력
```

만약 내일의 날짜를 계산하고 싶다면 `day()` 함수에 1을 더해주면 됩니다. 또한 `month()` 함수를 사용하면 현재 월을, `year()` 함수를 사용하면 현재 연도를 알 수 있습니다.

```Arduino
// 오늘의 날짜 계산
int day = day(); // 현재일을 day 변수에 저장
int month = month(); // 현재 월을 month 변수에 저장
int year = year(); // 현재 연도를 year 변수에 저장

// 내일의 날짜 계산
day = day + 1; // day 변수 값에 1을 더해줌
Serial.println(day + "/" + month + "/" + year); // 시리얼 모니터에 내일의 날짜 출력
```

하지만 날짜를 미래나 과거로 계산하는 것은 아주 복잡한 작업일 수 있습니다. 이럴 때는 `Time` 라이브러리를 사용하면 편리합니다. 이 라이브러리는 날짜와 시간을 설정하고 계산할 수 있는 다양한 함수를 제공합니다.

가장 먼저, 이 라이브러리를 사용하기 위해서는 `#include <Time.h>` 코드를 첫 줄에 추가해주어야 합니다. 그리고 `setTime()` 함수를 사용하여 날짜와 시간을 설정할 수 있습니다. 이 함수는 시간을 설정하는 데 필요한 초 단위의 값을 인수로 받습니다.

```Arduino
#include <Time.h> // Time 라이브러리 추가

// 원하는 날짜로 설정
setTime(0, 0, 0, 1, 1, 2022); // 2022년 1월 1일 0시 0분 0초로 설정
```

이제 `now()` 함수를 사용하면 현재 시간과 날짜를 계산할 수 있습니다. 이 함수는 `Time` 객체를 반환하며, 그 안에 다양한 함수를 사용할 수 있습니다. 예를 들어, `hour()` 함수는 현재 시간의 시를 반환하고, `day()` 함수는 현재 일을 반환합니다.

```Arduino
// 현재 시간 계산
Time now = now(); // 현재 시간을 now 변수에 저장

int hour = now.hour(); // 현재 시간의 시를 hour 변수에 저장
int minute = now.minute(); // 현재 시간의 분을 minute 변수에 저장
int second = now.second(); // 현재 시간의 초를 second 변수에 저장

// 현재 날짜 계산
int day = now.day(); // 현재 날짜를 day 변수에