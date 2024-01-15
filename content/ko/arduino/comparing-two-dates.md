---
title:                "두 날짜 비교하기"
html_title:           "Arduino: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

아두이노 프로그래밍에서 두 날짜를 비교하는 것은 특정 이벤트를 제어하거나 상황을 판별하는 데에 유용합니다.

## 어떻게

아래는 두 날짜를 비교하는 간단한 예제 코드입니다. 이 코드를 이용하여 원하는 날짜를 비교할 수 있습니다.

```Arduino 
#include <DS3231.h> // RTC Library
DS3231 rtc;

//Year, Month, Day, Hour, Minute, Second
DateTime date1(2020, 4, 5, 15, 30, 0); 
DateTime date2(2019, 8, 23, 18, 45, 0);

void setup() {
  Serial.begin(9600); // Serial Monitor를 위한 설정
  rtc.begin(); // RTC를 초기화합니다.
}

void loop() {
  DateTime now = rtc.now(); // 현재 날짜와 시간을 가져옵니다.

  // 두 날짜를 비교합니다.
  if(now < date1) {
    Serial.println("Date1은 현재 날짜보다 나중입니다.");
  } else if(now > date1) {
    Serial.println("Date1은 현재 날짜보다 이전입니다.");
  } else {
    Serial.println("Date1은 현재 날짜와 같습니다.");
  }

  // 다른 방법으로도 두 날짜를 비교할 수 있습니다.
  if(date2.isBefore(now)) {
    Serial.println("Date2는 현재 날짜보다 이전입니다.");
  } else if(date2.isAfter(now)) {
    Serial.println("Date2는 현재 날짜보다 나중입니다.");
  } else {
    Serial.println("Date2는 현재 날짜와 같습니다.");
  }
  
  delay(1000); // 1초마다 반복합니다.
}
```

위의 코드를 실행하면 아두이노의 시리얼 모니터에 두 날짜를 비교한 결과가 출력됩니다. 이를 통해 현재 날짜가 두 날짜 중 어떤 것과 비교해서 이전, 이후, 같음을 판별할 수 있습니다.

## 딥 다이브

아두이노의 RTC(Real-Time Clock) 라이브러리를 사용하면 두 날짜를 비교하는 것이 간단해집니다. 이 라이브러리는 DS1307 또는 DS3231과 같은 RTC 모듈과 함께 사용될 수 있습니다. 비교할 날짜는 DateTime 클래스로 생성할 수 있으며, 두 개의 DateTime 객체를 비교하여 작은지, 큰지, 같은지를 판별할 수 있습니다.

## See Also

- RTC 라이브러리: https://www.arduino.cc/en/Reference/RTC
- DS3231 RTC 모듈: https://www.adafruit.com/product/3013