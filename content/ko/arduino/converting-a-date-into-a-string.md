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

## 무엇이며 왜 사용하는가?

날짜를 문자열로 변환하는 것은 특정 날짜를 더 사람이 읽기 쉬운 텍스트 형태로 표현하는 작업을 말합니다. 이는 데이터를 사용자에게 표시하거나, 날짜 형식이 필요한 환경에 용이하게 적용할 수 있도록 하기 위해서 필요합니다.

## 방법:

아래는 Arduino를 사용하여 날짜를 문자열로 변환하는 예제입니다.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  time_t t = now(); // 현재 날짜와 시간을 받아옵니다.
  String dateString = String(year(t)) + "/" + String(month(t)) + "/" + String(day(t)); // 문자열로 변환합니다.

  Serial.println(dateString); // 변환한 문자열을 출력합니다.
  delay(1000);
}
```

이 코드를 실행하면, 시리얼 모니터에서 다음과 같이 출력됩니다:

```Arduino
2022/02/24
```

## 깊게 알아보기:

C++에는 기본적으로 날짜를 문자열로 변환하는 기능이 없습니다. 하지만 Arduino와 같은 환경에서는 통합 라이브러리를 통해 이를 쉽게 구현할 수 있습니다. 위의 예제에서 사용한 TimeLib 라이브러리는 그 중 하나입니다. 알ternative로는 RTClib와 같은 날짜와 시간 관련 라이브러리를 사용할 수 있습니다.

## 참고 자료:

- Arduino Time Library: <https://www.arduino.cc/reference/en/libraries/time/>
- Arduino RTC Library: <https://github.com/adafruit/RTClib>
- Arduino Tutorial on Date and Time: <https://startingelectronics.org/tutorials/arduino/arduino-DS1307-real-time-clock/>