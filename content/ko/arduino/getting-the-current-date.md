---
title:                "현재 날짜 가져오기"
html_title:           "Arduino: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

최신 버전의 아두이노 프로그래밍을 배우는 것은 아두이노 기반 제품을 개발하고자 하는 사람들에게 중요합니다. 현재 날짜를 얻는 방법은 프로그램의 일부로 사용될 수 있으며, 아두이노를 사용하는 동안 필수적인 기능입니다.

## 하우 투 (어떻게)

"아두이노 등록기능"을 이용하여 현재 날짜를 얻을 수 있습니다. 불러오기를 통해 해당 라이브러리를 불러온 다음, ```RTC``` 객체를 만듭니다. 그리고 아래 코드를 실행하여 ```year```, ```month```, ```day```, ```hour```, ```minute```, ```second``` 변수를 통해 현재 날짜를 가져올 수 있습니다.

```
#include <RTC.h>

RTC rtc;

int year, month, day, hour, minute, second;

void setup() {

  // 아두이노 등록기능 불러오기
  #include <RTClib.h>

  rtc = RTC();
}

void loop() {

  // 현재 날짜 변수 지정
  year = rtc.now().year();
  month = rtc.now().month();
  day = rtc.now().day();
  hour = rtc.now().hour();
  minute = rtc.now().minute();
  second = rtc.now().second();

  // 확인용 출력문
  Serial.println("Current Date:");
  Serial.print(month);
  Serial.print("/");
  Serial.print(day);
  Serial.print("/");
  Serial.print(year);
  Serial.print(" ");
  Serial.print(hour);
  Serial.print(":");
  Serial.print(minute);
  Serial.print(":");
  Serial.println(second);

  delay(1000); // 1초마다 날짜를 다시 가져옵니다.
}
```

위 코드를 실행하면 시리얼 모니터에 현재 날짜와 시간이 나타납니다. 만약 실제 디바이스에서 사용하고 싶다면, 현재 날짜 변수를 다른 작업에 활용할 수 있습니다.

## 딥 다이브

RTC 등록기능은 실제 시간을 추적하고 보존하기 위해 사용됩니다. 따라서 이를 사용하여 현재 날짜를 얻을 수 있습니다. RTC 등록기능은 몇 가지 다른 종류가 있지만, 가장 일반적으로 사용되는 것은 DS1307 모듈과 매우 유사합니다. DS1307 모듈은 아두이노와 연결되어 실제 시간을 추적하고 보존합니다. 이 기능은 아두이노를 사용하는 많은 프로젝트에서 필요합니다. 코드 예제에서 사용한 RTC 객체는 RTC 모듈을 초기화하기 위해 사용되는 간단한 함수입니다.

## 더 알아보기

- [RTC 등록기능 라이브러리 설명서](https://www.arduino.cc/en/Reference/RTC)
- [DS1307 모듈 사용 방법](https://www.arduino.cc/en/Tutorial/RTC)
- [Adafruit RTC 라이브러리](https://github.com/adafruit/RTClib)
- [실제 시간 추적 및 게임 개발에 대한 AI 라이브러리](https://github.com/QuestoGame/DateTime)