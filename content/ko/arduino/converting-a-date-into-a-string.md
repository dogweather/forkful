---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:35:55.069668-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-date-into-a-string.md"
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
