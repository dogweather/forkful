---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그런가요?

문자열 연결(Concatenating Strings)은 두 개 이상의 문자열을 하나로 결합하는 것을 말합니다. 프로그래머들은 이를 사용하여 여러 segment의 데이터를 한 줄의 메시지나 코드로 변환하는 데 사용합니다.

## 어떻게 하나요:

사용법을 이해하기 위해 아래의 아두이노 코드 예시를 살펴보세요.

```Arduino
String stringOne = "Hello";
String stringTwo = "Arduino";
stringOne += stringTwo;     // stringOne에 stringTwo를 연결
Serial.println(stringOne);  // "HelloArduino"를 출력
```
위 코드는 "Hello"라는 문자열과 "Arduino"라는 문자열을 사용하여 "HelloArduino"라는 문자열을 생성하고 Serial Monitor에 출력하는 데 사용됩니다.

## 깊게 알아보기:

문자열 연결은 1970년대부터 사용되어 왔으며, 이를 사용하면 저장 공간의 최적화와 코딩의 효율성을 높일 수 있습니다. 중요한 것은 문자열 연결은 메모리를 많이 사용하므로 아두이노 같은 하드웨어에서는 주의해야 합니다. 대안으로 `snprintf`과 같은 함수를 사용하여 메모리 사용량을 최소화하면서 문자열을 연결할 수 있습니다.

## 참고해 볼 만한 것:

원한다면 아래의 관련 자료를 확인하세요:
- 아두이노 공식 사이트의 문자열 수업(https://www.arduino.cc/en/Tutorial/StringAdditionOperator)
- C ++에 대한 문자열 연결에 대한 Stack overflow 토론(https://stackoverflow.com/questions/18892281/most-idiomatic-way-to-concatenate-strings-in-c-c11)