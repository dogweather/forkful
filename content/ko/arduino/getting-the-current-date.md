---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 얻는 것은 그 날짜를 변수로 저장하고 있음을 의미합니다. 이를 통해 프로그래머는 시간 경과를 추적하거나 날짜/시간 타임스탬프와 같은 정보를 로깅하는 등의 기능을 기본 코드에 적용할 수 있습니다.


## 어떻게:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);
  
  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
    // Following line sets the RTC to the date & time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop () {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
}
```
_예시 출력: 2022/12/22_

## 깊게 파보기

이 코드에서 사용된 RTC 라이브러리는 DS1307 실시간 클럭 모듈을 활용한 것입니다. 이 모듈은 2000년도 이후의 날짜/시간 정보를 메모리에 유지합니다. 이러한 방법 대신 시스템의 내부 시계를 이용해 날짜와 시간 정보를 가져오는 방법도 있습니다만, 이는 내부 시계의 정확성에 대한 의존도를 높이게 됩니다.

## 참고하기

훨씬 더 복잡한 날짜와 시간 조작을 위해 아두이노의 Time 라이브러리를 사용하려면 이쪽을 확인하세요: [아두이노 Time 라이브러리](https://www.arduino.cc/en/Reference.Time) 해당 라이브러리는 날짜와 시간 정보를 얻는 데 사용할 수 있는 여러 가지 함수를 제공합니다.

DS1307 모듈에 대해 더 알아보려면 아래를 참조하세요: [DS1307 실시간 클럭 모듈](http://playground.arduino.cc/Main/DS1307RealTimeClock)