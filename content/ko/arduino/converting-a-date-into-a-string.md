---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜?

일일 생활에서 날짜 정보를 문자열로 변환하는 일은 매우 흔합니다. 이러한 일은 주로 시간을 기록하거나 다른 형식으로 표현할 때 필요합니다. 아두이노 프로그래밍에서 날짜를 문자열로 변환하는 방법을 배우면 일상적인 작업을 더 쉽게 처리할 수 있습니다.

## 어떻게?

```Arduino
#include <RTClib.h> //RTC 라이브러리 import

RTC_DS1307 rtc; //RTC 객체 생성

void setup() {
  Serial.begin(9600); //시리얼 통신 시작
  while (!Serial) {
    //시리얼 연결 대기
  }

  if (! rtc.begin()) {
    //RTC 시작 실패 시 에러 메시지 출력
    Serial.println("RTC not found!");
    while (1);
  }

  if (rtc.lostPower()) {
    //RTC 전원이 끊어 졌을 때 기재된 시간 출력
    Serial.println("RTC lost power, let's set the time!");
    //다음 줄 부터 진행한 예제 참고
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now(); //RTC에서 현재 시간 가져오기

  Serial.print(now.year(), DEC); //현재 연도 출력
  Serial.print('/'); //별을 연결

  Serial.print(now.month(), DEC); //현재 월 출력
  Serial.print('/');

  Serial.print(now.day(), DEC); //현재 일 출력
  Serial.print(' ');

  Serial.print(now.hour(), DEC); //현재 시간 출력
  Serial.print(':');
 
  Serial.print(now.minute(), DEC); //현재 분 출력
  Serial.print(':');

  Serial.print(now.second(), DEC); //현재 초 출력
  Serial.println();

  delay(1000); //1초 대기
}
```

출력 예시:

```
2021/08/17 20:54:10
```

## 깊게 들어가기

일상적인 작업을 처리하는 동안 날짜 정보가 필요한 경우가 많습니다. 아두이노의 경우 날짜를 특정 형식으로 표현하거나 저장해야 할 때가 있습니다. 이때 날짜를 문자열로 변환하여 사용하는 것이 가장 간단하고 효율적인 방법입니다. 예제 코드에서 보듯이 RTC 모듈을 사용하여 현재 시간 정보를 가져와서 연도, 월, 일, 시간, 분, 초를 각각 출력하도록 설정합니다. 이를 통해 날짜 정보를 원하는 형식으로 쉽게 표현할 수 있습니다.

## 참고 자료

- [RTC 라이브러리](https://www.arduino.cc/reference/en/libraries/rtclib/)
- [TinkerCAD에서 아두이노와 RTC 모듈 사용하기](https://www.tinkercad.com/things/8mRgPmi0JVX-curious-wolt/editel?sharecode=cf4H4JYV0MEwFnMMy8-nMMi0RWZ8Ooo4CbdUZAmzqm8=)
- [Real-Time Clock (RTC) 모듈 소개 및 사용 방법](https://www.ardumotive.com/how-to-use-rtcrps-module-en.html)

## 더보기

- [아두이노 튜토리얼](https://www.arduino.cc/en/Tutorial/HomePage)
- [아두이노 공식 홈페이지](https://www.arduino.cc/)
- [아두이노 커뮤니티](https://forum.arduino.cc/)