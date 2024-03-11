---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:55.080407-07:00
description: "\uC544\uB450\uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C \uD604\
  \uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\uC740 \uB85C\uAE45, \uD0C0\uC784\uC2A4\
  \uD0EC\uD551 \uB610\uB294 \uC2A4\uCF00\uC904\uB9C1 \uC791\uC5C5\uC5D0 \uC911\uC694\
  \uD55C \uC2E4\uC2DC\uAC04 \uC815\uBCF4\uB97C \uC5BB\uB294 \uAC83\uC744 \uD3EC\uD568\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uAE30\
  \uB2A5\uC131\uC744 \uAC15\uD654\uD558\uACE0, \uB370\uC774\uD130\uC758 \uAD00\uB828\
  \uC131\uC744 \uBCF4\uC7A5\uD558\uBA70, IoT \uBC0F \uB0B4\uC7A5 \uD504\uB85C\uC81D\
  \uD2B8\uC5D0\uC11C \uC2DC\uAC04\uC5D0 \uBBFC\uAC10\uD55C \uC791\uC5C5\uC744 \uC6A9\
  \uC774\uD558\uAC8C \uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC774\u2026"
lastmod: '2024-03-11T00:14:29.550098-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C \uD604\uC7AC\
  \ \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\uC740 \uB85C\uAE45, \uD0C0\uC784\uC2A4\uD0EC\
  \uD551 \uB610\uB294 \uC2A4\uCF00\uC904\uB9C1 \uC791\uC5C5\uC5D0 \uC911\uC694\uD55C\
  \ \uC2E4\uC2DC\uAC04 \uC815\uBCF4\uB97C \uC5BB\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uAE30\uB2A5\
  \uC131\uC744 \uAC15\uD654\uD558\uACE0, \uB370\uC774\uD130\uC758 \uAD00\uB828\uC131\
  \uC744 \uBCF4\uC7A5\uD558\uBA70, IoT \uBC0F \uB0B4\uC7A5 \uD504\uB85C\uC81D\uD2B8\
  \uC5D0\uC11C \uC2DC\uAC04\uC5D0 \uBBFC\uAC10\uD55C \uC791\uC5C5\uC744 \uC6A9\uC774\
  \uD558\uAC8C \uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC774\u2026"
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
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
