---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:13:20.529518-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜 가져오기는 실시간으로 연, 월, 일 정보를 기록하는 것입니다. 이를 통해 프로그래머들은 이벤트 타이밍 추적, 데이터 로깅, 사용자 경험의 시간 관련 기능을 구현합니다.

## How to (어떻게 할까요?):
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, setting the time!");
    // Set the date and time to the date and time this sketch was compiled
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(1000);
}
```
Sample output:
```
2023/3/15
```

## Deep Dive (심층 분석):
현재 날짜를 가져오려면 실시간 클록(RTC) 모듈이 필요합니다. 이 예제에서는 DS3231 RTC를 사용했습니다. 왜냐하면 정확성과 배터리 백업 기능이 있기 때문입니다. 역사적으로, RTC는 하드웨어에 내장되어 PC 시계를 관리했습니다. 현재에는 비슷한 모듈을 아두이노 프로젝트에 쉽게 추가할 수 있습니다.

알터네이티브로, 네트워크 시간 프로토콜(NTP) 클라이언트를 사용해 인터넷에서 시간을 가져올 수 있지만, 인터넷 연결이 필요합니다. 다른 구현 방식으로는 GPS 모듈을 사용해 UTC 시간을 확득하는 방법이 있습니다.

실제 구현에서는 `RTClib` 라이브러리를 사용합니다. 이 라이브러리는 다양한 RTC 하드웨어와 인터페이스하도록 설계되었으며 간단한 API를 통해 날짜와 시간 정보에 접근을 용이하게 합니다. 코드에서 `rtc.now()` 함수를 호출하여 현재 날짜와 시간을 `DateTime` 객체로 받습니다. 이 객체로부터 년(`year()`), 월(`month()`), 일(`day()`)을 얻어낼 수 있습니다.

## See Also (참고 자료):
- RTClib 라이브러리: https://github.com/adafruit/RTClib
- DS3231 RTC 모듈: https://datasheets.maximintegrated.com/en/ds/DS3231.pdf
- NTP 클라이언트 구현 방법: https://www.arduino.cc/en/Tutorial/LibraryExamples/NTPClient
- 아두이노와 GPS 시간 동기화: https://www.arduino.cc/en/Tutorial/GPSTimeSync
