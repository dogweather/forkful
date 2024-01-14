---
title:                "Arduino: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 방법을 살펴보는 것은 Arduino 프로그래밍에서 중요한 기술입니다. 날짜를 문자열로 변환함으로써 날짜 형식의 유연성이 크게 향상되며, 다양한 출력 방식을 효과적으로 처리할 수 있습니다.

## 방법

아래의 ```Arduino``` 코드 블록에서는 날짜를 문자열로 변환하는 간단한 예제를 살펴볼 수 있습니다. 이 코드를 실행하면 날짜를 원하는 형식의 문자열로 출력할 수 있습니다.

```Arduino
// 현재 날짜를 얻기 위해 time 라이브러리를 사용합니다.
#include <TimeLib.h>

void setup() {
  // USB 시리얼 통신을 활성화합니다.
  Serial.begin(9600);

  // 시간 라이브러리를 초기화합니다.
  setTime(0, 0, 0, 1, 1, 2020); // (시, 분, 초, 일, 월, 년)

  // 날짜를 문자열로 변환하여 출력합니다.
  Serial.println( String(dateToStr(month()), DEC) + "/" + String(dateToStr(day()), DEC) + "/" + String(dateToStr(year()), DEC) );

}

void loop() {

}

```

위의 코드에서는 시간 라이브러리의 ```dateToStr()``` 함수를 사용하여 날짜를 문자열로 변환합니다. 이 함수는 날짜 값을 첫 번째 매개변수로 받으며, 두 번째 매개변수인 수의 밑수(base)를 지정할 수 있습니다. 위의 예제에서는 10진수로 변환하여 출력하도록 설정하였습니다.

출력 결과는 다음과 같습니다.

```
1/1/2020
```

이처럼, 날짜를 문자열로 변환하는 데에는 직접적인 방법 외에도 다양한 방법이 있습니다. 예를 들어, 유니버셜 코디트 시간(Unix Epoch Time)과 같은 특정 형식의 날짜 값을 문자열로 변환하는 것도 가능합니다. 이에 대한 자세한 내용은 더 깊은 탐구(Deep Dive) 섹션에서 살펴보겠습니다.

## 더 깊은 탐구

날짜를 문자열로 변환하는 것은 Arduino 프로그래밍에서 자주 사용되는 기술 중 하나입니다. 따라서 다양한 방법이 존재하며, 이해하는 것이 중요합니다.

시간 라이브러리에서는 날짜 형식을 사용자가 원하는 대로 출력할 수 있는 다양한 함수를 제공합니다. 이 외에도 날짜 및 시간 관련 라이브러리를 추가하거나, 직접 날짜 값을 특정 형식의 문자열로 변환하는 함수를 작성할 수도 있습니다. 따라서 날짜를 문자열로 변환하는 방법에 대한 탐구를 통해 Arduino 프로그래밍 실력을 증진시킬 수 있습니다.

## 더 알아보기

[Time 라이브러리](https://www.arduino.cc/reference/en/libraries/time/)  

[Time 라이브러리 예제](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Time)  

[날짜 형식 관련 라이브러리 참고](https://www.arduino.cc/en/Tutorial/SerialCallResponseASCII)