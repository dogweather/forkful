---
date: 2024-01-20 17:35:55.069668-07:00
description: "How to: (\uBC29\uBC95) \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\
  \uD658\uD558\uB294 \uAC83\uC740 \uCEF4\uD4E8\uD130 \uD504\uB85C\uADF8\uB798\uBC0D\
  \ \uCD08\uCC3D\uAE30\uBD80\uD130 \uC788\uC5C8\uB358 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  . Arduino\uC5D0\uC11C\uB294 `RTClib.h` \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD558\uC5EC \uC2E4\uC2DC\uAC04 \uD074\uB85D(RTC)\uC5D0\uC11C\
  \ \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uC77D\uC740 \uB2E4\uC74C `snprintf()` \uD568\
  \uC218\uB85C \uD3EC\uB9E4\uD305\uD569\uB2C8\uB2E4. \uB300\uC548\uC73C\uB85C\uB294\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.883632-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740 \uCEF4\uD4E8\uD130 \uD504\uB85C\uADF8\uB798\uBC0D \uCD08\uCC3D\
  \uAE30\uBD80\uD130 \uC788\uC5C8\uB358 \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

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
