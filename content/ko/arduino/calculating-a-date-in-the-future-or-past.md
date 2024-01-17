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

## 무엇 & 왜? 
날짜 계산이란 미래나 과거의 날짜를 계산하여 결과를 출력하는 것을 말합니다. 프로그래머들은 이를 사용하여 자신이 원하는 날짜를 정확하게 계산할 수 있습니다.

## 어떻게:
```
Arduino에서는 내장 함수를 사용하여 날짜를 계산할 수 있습니다. 다음은 간단한 예제 코드와 결과입니다. 
```
```Arduino
#include <Time.h> // Time 라이브러리 불러오기

void setup() {
  TimeElements futureDate; // 미래 날짜를 저장할 객체 생성

  futureDate.Year = 2019; // 년도 지정 (2000-2069 범위 내)
  futureDate.Month = 12; // 월 지정 (1-12 범위 내)
  futureDate.Day = 25; // 일 지정(1-31 범위 내)

  time_t futureTime = makeTime(futureDate); // TimeElements 객체를 time_t 형식으로 변환
  Serial.println(dayStr(weekday(futureTime))); // 해당 날짜의 요일 출력
  Serial.println(dayShortStr(weekday(futureTime))); // 요일의 약어 출력
  Serial.println(monthStr(month(futureTime))); // 해당 날짜의 월 출력
  Serial.println(monthShortStr(month(futureTime))); // 월의 약어 출력
  Serial.println(day(futureTime)); // 해당 날짜의 일 출력
  Serial.println(year(futureTime)); // 해당 날짜의 년도 출력
}
void loop() {
  
}
```
```
## Deep Dive:
- Arduino에서는 Time 라이브러리를 통해 날짜와 시간을 관리할 수 있습니다.
- 이 라이브러리는 Unix 시스템에서 사용하는 time_t 형식을 지원합니다.
- 만약 내장 함수를 사용하지 않고 직접 날짜를 계산하려면, 윤년과 월의 일수를 고려해야 합니다.
```

## See Also:
- [Time 라이브러리 문서](https://www.arduino.cc/en/Reference/Time)
- [날짜 및 시간 관리를 위한 여러 라이브러리](https://playground.arduino.cc/Code/Time/)