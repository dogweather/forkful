---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:55.080407-07:00
description: "\uBC29\uBC95: \uC544\uB450\uC774\uB178 \uC790\uCCB4\uB294 \uC2E4\uC2DC\
  \uAC04 \uC2DC\uACC4(RTC)\uAC00 \uC5C6\uC5B4 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC9C1\
  \uC811 \uAC00\uC838\uC62C \uC218 \uC788\uB294 \uB0B4\uC7A5 \uBA54\uC11C\uB4DC\uAC00\
  \ \uC5C6\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098, DS3231\uACFC \uAC19\uC740 \uC678\
  \uBD80 RTC \uBAA8\uB4C8\uACFC Adafruit\uC5D0 \uC758\uD574 \uAC1C\uBC1C\uB41C `RTClib`\uACFC\
  \ \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774\
  \uB97C \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.623128-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178 \uC790\uCCB4\uB294 \uC2E4\uC2DC\uAC04 \uC2DC\uACC4\
  (RTC)\uAC00 \uC5C6\uC5B4 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC9C1\uC811 \uAC00\uC838\
  \uC62C \uC218 \uC788\uB294 \uB0B4\uC7A5 \uBA54\uC11C\uB4DC\uAC00 \uC5C6\uC2B5\uB2C8\
  \uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
아두이노 자체는 실시간 시계(RTC)가 없어 현재 날짜를 직접 가져올 수 있는 내장 메서드가 없습니다. 그러나, DS3231과 같은 외부 RTC 모듈과 Adafruit에 의해 개발된 `RTClib`과 같은 라이브러리를 사용하여 이를 달성할 수 있습니다. 이 라이브러리를 사용하면 이러한 모듈과의 인터페이싱을 간단하게 만듭니다.

먼저, `RTClib` 라이브러리가 아두이노 IDE에 설치되어 있는지 확인하십시오. 그런 다음 RTC 모듈을 해당 문서에 따라 아두이노에 연결하십시오.

시작하기 위한 간단한 예제를 소개합니다:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("RTC를 찾을 수 없습니다");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC가 전원을 잃었습니다, 시간을 설정합시다!");
    // 새 장치에서 시간을 설정하거나 전원 손실 후에 시간을 설정해야 할 때, 여기서 설정할 수 있습니다.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("현재 날짜: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // 시리얼 스팸을 줄이기 위해 3초 지연
}
```

샘플 출력 (RTC가 사전에 설정되었다고 가정):

```
현재 날짜: 2023/4/15
```

이 코드는 RTC 모듈을 초기화한 다음, 루프에서 현재 날짜를 매 3초마다 시리얼 모니터에 가져와서 출력합니다. `rtc.adjust(...)` 줄은 주석 처리를 제거하고 수정하여 처음이나 전원이 손실된 후 RTC의 날짜와 시간을 설정할 수 있음을 기억하십시오.
