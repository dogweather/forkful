---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 날짜를 구문 분석하는 것은 문자열로부터 날짜 데이터를 추출하는 과정입니다. 이는 주로 텍스트 데이터에서 날짜 정보를 효율적으로 처리하고 데이터를 정렬하거나 사용하기 위해 프로그래머들이 사용합니다.

## 사용 방법:

문자열에서 날짜를 구문 분석하기 위한 간단한 Arduino 코드 예제입니다.

```Arduino
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  DateTime now = RTC.now();
  
  String dateString = String(now.day()) + "/" + String(now.month()) + "/" + String(now.year());
  Serial.println(dateString);

  delay(1000);
}
```

이 코드를 실행하면, 스트링(`dateString`)으로 읽혀진 현재의 날짜를 출력할 것입니다.

## 딥 다이브:

#### 1. 역사적 맥락
많은 프로그래밍 언어가 문자열을 구문 분석하는 함수를 이미 제공하고 있지만, 아두이노와 같은 제한된 자원의 마이크로 컨트롤러에서는 효율적인 문자열 처리가 중요합니다.

#### 2. 대체 방안
다른 방법으로는 문자열을 나누는 기능을 사용하여 날짜, 월, 년으로 나누는 방법이 있습니다. 

#### 3. 구현 세부 사항
이 예제에서는 `RTClib` 라이브러리를 사용하여 현재 시간을 가져와서 문자열로 변환합니다. 변환된 문자열은 날짜/월/년 형식을 따릅니다.

## 참고 자료:

여기에는 문자열에서 날짜를 구문 분석하는 방법과 관련된 추가 정보를 찾을 수 있는 몇 가지 링크가 있습니다.
- RTE 라이브러리: [RTClib Library](https://github.com/adafruit/RTClib)
- Arduino 활용 가이드: [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)