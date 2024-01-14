---
title:                "Arduino: 현재 날짜 받기"
simple_title:         "현재 날짜 받기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

현재 날짜를 가져오는 것은 매우 유용한 일입니다. 우리는 일상 생활에서 날짜를 자주 필요로 하며, 이를 자동으로 얻을 수 있다면 매우 편리합니다. 또한, 프로젝트에 따라 특정 날짜를 설정해야 할 경우에도 매우 유용합니다.

# 어떻게

아두이노에서 현재 날짜를 가져오려면 `RTC` (Real-Time Clock) 모듈을 사용해야 합니다. 이 모듈은 아날로그 신호를 디지털로 변환하여 시간과 날짜를 제공합니다. 아래는 RTC 모듈을 사용하여 현재 날짜 정보를 얻는 예시 코드입니다.

```Arduino
// RTC 모듈을 사용하기 위해 Wire 라이브러리를 가져옵니다.
#include <Wire.h>
// RTC 모듈을 사용하기 위해 RTClib 라이브러리를 가져옵니다.
#include <RTClib.h>

// RTC 모듈을 연결할 핀 번호를 지정합니다.
#define RTC_SDA 4
#define RTC_SCL 5

// RTC 객체를 생성합니다.
RTC_DS1307 rtc;

void setup() {
  // Serial 통신을 시작합니다.
  Serial.begin(9600);
  // RTC 모듈을 초기화합니다.
  rtc.begin();
  // RTC 모듈을 사용하기 위해 Wire 라이브러리에 핀 번호를 알려줍니다.
  Wire.begin(RTC_SDA, RTC_SCL);
}

void loop() {
  // RTC 모듈에서 날짜 정보를 가져옵니다.
  DateTime now = rtc.now();
  // 날짜 정보를 출력합니다.
  Serial.print(now.year()); // 현재 연도
  Serial.print('/'); // 연도와 월을 구분하기 위해 슬래시를 출력합니다.
  Serial.print(now.month()); // 현재 월
  Serial.print('/');
  Serial.print(now.day()); // 현재 일
  // 참고: 지정한 날짜 형식에 따라 월과 일이 한 자리일 경우 앞에 0이 들어갈 수 있습니다.
  Serial.println();
  // 1초 간격으로 출력합니다.
  delay(1000);
}
```

위 코드를 실행하면 시리얼 모니터에서 현재 날짜 정보를 확인할 수 있습니다. 예를 들어, 2021년 11월 4일일 때 `2021/11/4`와 같이 출력됩니다.

# 딥 다이브

아두이노에서는 RTC 모듈을 통해 `DateTime` 객체를 사용하여 시간과 날짜 정보를 다룹니다. 이 객체에는 연도, 월, 일, 시간, 분, 초 등의 정보를 저장할 수 있습니다. 또한, `now.unixtime()` 함수를 사용하면 현재 시간부터 1970년 1월 1일 자정까지의 초를 계산하여 반환할 수 있습니다. 이를 활용하여 시간 관리 프로그램 등 다양한 프로젝트를 구현할 수 있습니다.

# 참고

- [Arduino RTC 예제 코드](https://www.arduino.cc/en/Tutorial/RTC)
- [RTC 동작 원리 및 예제 코드 설명](https://www.arduino.cc/reference/en/libraries/rtc/)

## 또 다른 자료

- [아두이노 공식 사이트](https://www.arduino.cc/)