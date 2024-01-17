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

## 무엇 & 왜?
날짜를 문자열로 변환하는 것이 무엇인지와 프로그래머들이 왜 이것을 하는지를 설명하는 두 문장입니다.

날짜를 문자열로 변환하는 것은 날짜 데이터를 더 쉽게 읽고 처리할 수 있도록 하는 것입니다. 예를 들어, 날짜를 문자열로 변환하면 출력할 때 형식을 정할 수 있으며, 날짜가 위 숫자로만 이루어져 있다면 의미를 해석하기 어려울 수 있기 때문입니다.

## 어떻게:
```Arduino ... ``` 코드 블록 안에 코딩 예제와 샘플 출력입니다.

```C++
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);

  while (!Serial) {
    delay(1);
  }

  if (! rtc.begin()) {
    Serial.println("Couldn't find RTC");
    Serial.flush();
    abort();
  }

  if (! rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
  }

  DateTime now = rtc.now();

  // 날짜를 MM/DD/YYYY 포맷의 문자열로 변환
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print('/');
  Serial.println(now.year(), DEC);

  // 날짜를 DD-MM-YYYY HH:MM:SS 포맷의 문자열로 변환
  Serial.print(now.day(), DEC);
  Serial.print('-');
  Serial.print(now.month(), DEC);
  Serial.print('-');
  Serial.print(now.year(), DEC);
  Serial.print(' ');
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  if (now.minute() < 10) {
    Serial.print('0');
  }
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  if (now.second() < 10) {
    Serial.print('0');
  }
  Serial.println(now.second(), DEC);
}

void loop () {
  // 아무것도 하지 않음
}
```

### 샘플 출력:
```
2/4/2022
4-2-2022 12:35:38
```

이 코드는 RTC (Real Time Clock) 모듈을 사용하여 현재 날짜와 시간을 얻어온 다음, ```DateTime``` 객체를 활용해 문자열로 변환하는 방법을 보여줍니다. 처음의 변환 결과는 MM/DD/YYYY, 두 번째의 변환 결과는 DD-MM-YYYY HH:MM:SS 포맷으로 출력됩니다.

## 심화 분석:
날짜를 문자열로 변환하는 방법은 많이 있지만, 대부분의 프로그래머들은 위 예제처럼 ```DateTime``` 객체를 사용해 변환하는 방식을 선호합니다. 이는 날짜와 시간을 처리해주는 라이브러리가 꽤 활발한 편이기 때문입니다. 또한, 마이크로컨트롤러 위에서 돌아가는 시스템에서는 여러 외부 라이브러리를 사용하는 것보다 내장 함수와 코드를 사용하는 것이 성능상 더 좋을 수 있습니다.

## 관련 링크:
- [DateTime 라이브러리 문서](https://www.arduino.cc/reference/en/libraries/rtc/)
- [마이크로컨트롤러 시간 관리 방법 비교 글](http://www.gammon.com.au/forum/?id=12160)