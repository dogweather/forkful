---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 하는가?

문자열 보간(Interpolating a string)이란 변수, 상수 또는 문자열 자체 내에 표현식을 삽입하는 코딩 작업을 말합니다. 이는 코드를 보다 간결하고 가독성이 좋게 만들기 위하여 프로그래머들이 자주 사용합니다.

## 어떻게 하는가:

아래는 Arduino에서 문자열 보간을 수행하는 법에 대한 코드 예시입니다.

```Arduino 
String sensor = "Temperature";
int value = 25;

String message = "The " + sensor + " is: " + String(value);
Serial.println(message);

// Sample output: The Temperature is: 25
```
이런 방식으로, 우리는 바로 변수 및 상수 값을 문자열로 변환하고 합칠 수 있습니다.

## Deep Dive:

1. **역사적 맥락**: 최초의 문자열 보간은 1960년대에 개발된 SNOBOL4 프로그래밍 언어에서 처음 도입되었습니다. 최근에는 많은 프로그래밍 언어가 이 기능을 포함하고 있습니다.

2. **대체 방안**: `sprintf` 함수는 문자열 보간의 대안으로 사용될 수 있지만, 그 사용은 메모리 관리 문제로 인해 Arduino에서는 권장되지 않습니다.

3. **구현 세부사항**: Arduino에서의 문자열 보간은 `String` 클래스의 연결 연산자를 사용하여 수행됩니다. 이는 C++에서 연산자 오버로딩을 이용하여 구현되었습니다.

## 참고 자료:

1. Arduino 공식 사이트에서 `String` 클래스에 대한 자세한 정보: [Click here](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. 문자열 보간에 관한 Wikipedia 설명: [Click here](https://en.wikipedia.org/wiki/String_interpolation)