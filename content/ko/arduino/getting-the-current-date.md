---
title:    "Arduino: 현재 날짜 가져오기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜: 

현재 날짜를 얻는 것이 중요한 이유는 무엇일까요? 우리는 날짜를 보고 일정을 계획하고, 시간을 지키고, 그리고 다가오는 일정을 준비하기 위해 필요합니다. 또한, 특정 날짜에 따라서 특별한 코드 동작을 하게 할 수도 있습니다.

## 코딩하기:

```arduino
#include <RTClib.h> 

RTC_DS3231 rtc;  //RTC 라이브러리 사용

void setup () { 
//RTC 라이브러리 초기화
rtc.begin(); 

//날짜, 시간 설정
rtc.adjust(DateTime(yyyy, mm, dd, hh, mm, ss));

//현재 날짜를 출력
Serial.print(rtc.now().day(), DEC);
Serial.print('/');
Serial.print(rtc.now().month(), DEC);
Serial.print('/');
Serial.print(rtc.now().year(), DEC);
}

void loop () {
  //코드를 반복적으로 실행
}
```

위의 코드는 현재 RTC 모듈을 사용하여 날짜를 얻는 방법을 단계별로 보여줍니다. 먼저 RTC 라이브러리를 사용하여 모듈을 초기화하고, 원하는 날짜 및 시간을 설정합니다. 그리고 `rtc.now()` 함수를 사용하여 현재 날짜를 출력할 수 있습니다.

## 깊이 이해하기:

현재 날짜를 얻는 방법은 매우 다양한데, RTC 모듈을 사용하는 것이 가장 간편한 방법입니다. RTC 모듈은 시스템 전원이 꺼진 상태라도 시간과 날짜 정보를 유지할 수 있기 때문에 정확한 날짜를 얻을 수 있습니다. 또한 RTC 모듈을 사용하면 매번 사용자가 날짜를 설정하지 않아도 되기 때문에 편리합니다.

## 더 자세한 정보:

먼저 RTC 모듈을 사용하기 전에 반드시 라이브러리를 설치해야 합니다. 또한 RTC 모듈의 핀 배치 등 자세한 정보는 아두이노 공식 문서나 관련 인터넷 자료들을 참고하시면 도움이 됩니다.

## 더 알아보기:

- [아두이노 공식 문서 - RTC 라이브러리 사용하기](https://www.arduino.cc/en/Reference/RTC)
- [아두이노 포럼 - RTC 모듈에 관한 질문과 답변](https://forum.arduino.cc/index.php?board=2.0)
- [Adafruit Industries - RTC 모듈 관련 가이드](https://learn.adafruit.com/adafruit-ds3231-precision-rtc-breakout)