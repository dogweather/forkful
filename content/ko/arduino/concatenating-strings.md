---
title:                "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜?
여러분은 아두이노 프로그래밍을 할 때 문자열을 합쳐야 할 때가 있습니다. 예를 들어, 센서에서 수집한 데이터를 문자열로 출력하거나, 사용자와의 인터랙션을 위해 텍스트를 생성하는 경우 등이 있을 수 있습니다. 문자열을 합치면 더 간편하고 효율적으로 코딩할 수 있습니다.

## 어떻게?
우선 아두이노 보드와 시리얼 통신을 위해 컴퓨터에 코드를 업로드합니다. 그 다음, concatenate라고 불리는 함수를 사용하여 두 개의 문자열을 합쳐보겠습니다. 여러분은 아래 예시 코드를 따라해보면서 쉽게 연습할 수 있습니다. 

```Arduino
String hello = "Hello";
String name = "World";
String message = hello + " " + name; // "Hello World"
```

위 코드에서는 먼저 `hello`라는 변수에 "Hello"라는 값을, `name`이라는 변수에는 "World"라는 값을 할당했습니다. 그리고 이 두 변수를 `+` 기호로 연결하여 `message`라는 변수에 저장했습니다. 이제 `Serial.println()` 함수를 사용하여 `message`를 시리얼 모니터에 출력하면 "Hello World"라는 결과값이 나올 것입니다. 

## 깊이 들어가기
위 예시에서 보듯이 거의 모든 문자열 연산에는 `+` 연산자가 사용됩니다. 이 `+` 연산자를 사용하여 문자열을 합칠 수 있지만, 다른 방식으로 합치는 것도 가능합니다. 예를 들어, `String.concat()` 함수를 사용하면 특정 문자열 뒤에 다른 문자열을 붙일 수 있습니다. 또는 `String +=`와 같이 `+=` 연산자를 사용해서도 문자열을 합칠 수 있습니다. 이들 함수와 연산자를 조합하여 여러분들만의 편리한 방식으로 문자열을 연결해보세요!

## 관련 정보
- [Arduino 공식 사이트](https://www.arduino.cc/)
- [Arduino 문자열 연산 관련 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [String 메서드와 활용법](https://www.arduino.cc/en/Reference/StringObject)