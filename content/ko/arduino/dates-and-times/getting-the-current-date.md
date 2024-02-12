---
title:                "현재 날짜 가져오기"
aliases: - /ko/arduino/getting-the-current-date.md
date:                  2024-02-03T19:08:55.080407-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
아두이노 프로젝트에서 현재 날짜를 얻는 것은 로깅, 타임스탬핑 또는 스케줄링 작업에 중요한 실시간 정보를 얻는 것을 포함합니다. 프로그래머들은 종종 기능성을 강화하고, 데이터의 관련성을 보장하며, IoT 및 내장 프로젝트에서 시간에 민감한 작업을 용이하게 하기 위해 이 기능이 필요합니다.

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
