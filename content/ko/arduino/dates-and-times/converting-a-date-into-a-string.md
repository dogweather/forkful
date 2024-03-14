---
date: 2024-01-20 17:35:55.069668-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC74, '2023\uB144 3\uC6D4 15\uC77C' \uAC19\uC740 \uB0A0\uC9DC\uB97C \uD14D\
  \uC2A4\uD2B8 '2023-03-15'\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4\
  . \uC774\uB807\uAC8C \uD558\uBA74 \uB0A0\uC9DC \uB370\uC774\uD130\uB97C \uD654\uBA74\
  \uC5D0 \uD45C\uC2DC\uD558\uAC70\uB098 \uBB38\uC790 \uD615\uC2DD\uC73C\uB85C \uC800\
  \uC7A5\uD558\uACE0 \uACF5\uC720\uD558\uAE30 \uC27D\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.624696-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\uB294\
  \ \uAC74, '2023\uB144 3\uC6D4 15\uC77C' \uAC19\uC740 \uB0A0\uC9DC\uB97C \uD14D\uC2A4\
  \uD2B8 '2023-03-15'\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uC774\
  \uB807\uAC8C \uD558\uBA74 \uB0A0\uC9DC \uB370\uC774\uD130\uB97C \uD654\uBA74\uC5D0\
  \ \uD45C\uC2DC\uD558\uAC70\uB098 \uBB38\uC790 \uD615\uC2DD\uC73C\uB85C \uC800\uC7A5\
  \uD558\uACE0 \uACF5\uC720\uD558\uAE30 \uC27D\uC2B5\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환한다는 건, '2023년 3월 15일' 같은 날짜를 텍스트 '2023-03-15'로 바꾸는 과정입니다. 이렇게 하면 날짜 데이터를 화면에 표시하거나 문자 형식으로 저장하고 공유하기 쉽습니다.

## How to: (방법)
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println(F("Couldn't find RTC"));
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println(F("RTC lost power, let's set the time!"));
    // Following line sets the RTC to the date & time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  char dateStr[11];
  snprintf(dateStr, sizeof(dateStr), "%04d-%02d-%02d", now.year(), now.month(), now.day());
  Serial.println(dateStr);

  delay(1000);
}
```

Sample output:
```
2023-03-15
```

## Deep Dive (신중한 분석)
날짜를 문자열로 변환하는 것은 컴퓨터 프로그래밍 초창기부터 있었던 작업입니다. Arduino에서는 `RTClib.h` 같은 라이브러리를 사용하여 실시간 클록(RTC)에서 날짜와 시간을 읽은 다음 `snprintf()` 함수로 포매팅합니다. 대안으로는 `String` 객체와 문자열 연결을 사용할 수 있지만, 이는 메모리를 많이 사용하고 느리므로 비효율적일 수 있습니다.

## See Also (더 보기)
- [RTClib GitHub Repository](https://github.com/adafruit/RTClib)
