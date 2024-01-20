---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Arduino: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 하는가?

미래나 과거의 날짜를 계산하는 것은 특정 기준일로부터 일정 기간이 지난 후의 날짜나 이전의 날짜를 찾는 행위를 말합니다. 이는 예약 시스템, 일정 계획, 날씨 예측 등 다양한 프로그램에서 필요합니다.

## 어떻게 하나:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(12, 0, 0, 1, 1, 2020); // 시간 설정: 오후 12시, 2020년 1월 1일
}

void loop() {
  time_t t = now();
  Serial.println("현재 시간: " + String(hour(t)) + ":" + String(minute(t)) + ":" + String(second(t)) + ", " + String(day(t)) + "/" + String(month(t)) + "/" + String(year(t)));
  
  t = t + SECS_PER_DAY * 7; // 일주일 후
  Serial.println("7일 후의 시간: " + String(hour(t)) + ":" + String(minute(t)) + ":" + String(second(t)) + ", " + String(day(t)) + "/" + String(month(t)) + "/" + String(year(t)));
  
  delay(1000);
}
```

## 딥 다이브

미래나 과거의 날짜 계산은 컴퓨터 시스템의 탄생 이래로 항상 필요한 기능 중 하나였습니다. 주요한 대안으로는 Unix timestamp 방식이 있습니다. This represents the number of seconds since January 1, 1970, which allows for easy calculation of future and past dates. Arduino에서는 위의 예제처럼 TimeLib 라이브러리를 사용하여 날짜 계산을 할 수 있습니다. 이 라이브러리는 "now" 함수를 통해 현재 시간을 제공하며, 'SECS_PER_DAY'와 같은 상수를 통해 시간 계산을 용이하게 합니다.

## 참고할 만한 자료

- [Arduino - TimeLib Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Date and Time Programming](https://en.wikipedia.org/wiki/Date_and_time_notation_in_Asia#South_Korea)