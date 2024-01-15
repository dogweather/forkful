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

## 왜
문자열을 연결하는 것(Concatenating strings)의 이유는 때때로 한번에 여러 값을 출력하는 것이 더 유용 할 수 있습니다.

## 어떻게
문자열을 연결하는 방법은 아래의 코드 블록을 이용해 다음과 같이 표현할 수 있습니다:

```Arduino
// "Hello"와 "world" 문자열을 연결하여 "Hello world" 출력
Serial.println("Hello" + "world");
```

아두이노에서는 "+"를 이용하여 두 문자열을 연결할 수 있습니다. 위의 코드를 실행하면 "Helloworld"라는 결과값이 출력됩니다. 

## 깊이 파고들기
문자열을 연결할 때 주의해야할 점이 있습니다. 예를 들어, 아래의 예제 코드를 살펴보겠습니다.

```Arduino
// "Hello"와 "world" 문자열 연결
String str = "Hello" + "world"; 
```

위의 코드는 예상하지 못한 결과를 출력합니다. "str" 변수에는 "Helloworld"가 아닌 "Hello0"라는 값을 가지게 됩니다. 이는 "+" 연산자가 두 문자열을 단순히 붙여서 출력하는 것이 아니라, 두 문자열의 길이를 참조하여 새로운 배열을 만들어주기 때문입니다. 이러한 경우 "+" 대신에 "concat()" 함수를 이용하여 문자열을 연결하는 것이 좋습니다.

또한, 아두이노에서는 문자열을 저장할 수 있는 메모리 공간이 제한적입니다. 따라서, 많은 양의 문자열을 연결하게되면 프로그램이 충돌할 수 있으므로 주의해야 합니다.

## 같이 보기
- [String concatenation in Arduino]("https://www.arduino.cc/en/Tutorial/StringAdditionOperator"): 아두이노 공식 사이트에서 제공하는 문자열 연결에 관련된 튜토리얼입니다.
- [JavaScript String concatenation]("https://www.w3schools.com/jsref/jsref_concat_string.asp"): 아두이노에서 사용하는 "+" 연산자와 비슷한 기능을 하는 JavaScript의 문자열 연결 방법을 살펴볼 수 있습니다.
- [Tips for improving Arduino strings]("https://www.sweetpeaswap.com/tips-for-improving-arduino-strings/"): 아두이노에서 문자열을 다루는 방법에 대한 팁을 제공하는 블로그 글입니다.