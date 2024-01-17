---
title:                "현재 날짜 받아오기"
html_title:           "Arduino: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 뭐니 & 왜? ##
Arduino에서 현재 날짜를 얻는 것은 매우 유용한 작업입니다. 이를 통해 시간 기반 기능을 구현하거나 이벤트 기반 프로그램을 작성할 수 있습니다.

## 어떻게 하시나요? ##
```
#include <TimeLib.h>  // 라이브러리 추가
#include <Timezone.h>

void setup() {
  Serial.begin(9600);  // 시리얼 통신 시작
  setSyncProvider(RTC.get);  // 싱크 제공자 설정
  Serial.println("Current Date: ");
  Serial.println(year());  // 현재 년도 출력
  Serial.println(month());  // 현재 월 출력
  Serial.println(day());  // 현재 일 출력
}

void loop() {
  // 다른 작업 수행
}
```

**Output:**
```
Current Date:
2021
8
30
```

## 깊이 들어가기 ##
1. **역사적 배경:** Arduino에서 현재 날짜를 얻는 방법은 RTC(Real Time Clock) 모듈을 이용해 몇 년 전부터 사용되어왔습니다. 최근에는 WiFi 모듈이나 인터넷에 연결하여 NTP(Network Time Protocol)를 이용해 실시간 시간을 얻는 방법이 더 많이 사용되고 있습니다.
2. **대안:** RTC 모듈을 이용하는 방법 외에도, 인터넷에 연결하여 NTP를 이용하는 방법이 있습니다. 또한, 외부의 시간 서버와 연결하여 현재 날짜를 가져올 수도 있습니다.
3. **구현 세부사항:** RTC 모듈을 이용하는 경우, 모듈에 연결된 전지를 교체해야할 수도 있고, 모듈의 설정이 잘못되어있을 경우 정확한 시간을 얻지 못할 수도 있습니다. 인터넷을 통해 NTP를 이용하는 경우, 인터넷 연결에 의존하므로 네트워크 상황에 따라 정확한 시간을 얻지 못할 수 있습니다.

## 관련 자료 ##
- Time.h 라이브러리: https://playground.arduino.cc/Code/Time 
- Timezone.h 라이브러리: https://github.com/JChristensen/Timezone 
- 블로그 포스팅: https://blog.arduino.cc/2020/03/17/how-to-get-current-date-and-time-with-arduino/