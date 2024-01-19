---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
문자열 길이를 찾는 것이란, 문자열에 얼마나 많은 문자가 포함되어 있는지를 세는 것을 의미합니다. 프로그래머들이 이를 수행하는 이유는 메모리 관리, 문자열 조작, 효과적인 데이터 처리를 위해서입니다.

## 어떻게 하는가:
아두이노에서 문자열의 길이를 찾는 가장 일반적인 방법은 `length()` 함수를 사용하는 것입니다.
```Arduino
String myString = "Arduino programming";
int len = myString.length();
Serial.println(len);
```
위 코드를 실행하면, `Serial Monitor`에는 숫자 `19`가 출력되어, 문자열에 총 19개의 문자가 있다는 것을 나타냅니다.

## Deep Dive
문자열의 길이를 찾는 이 기능이 처음 도입된 것은 역사적으로 초기 컴퓨터 과학 분야와 밀접한 관련이 있습니다. 컴퓨터 메모리 관리와 효과적인 데이터 처리를 위한 중요한 도구였습니다.
하지만 오늘날에는 아두이노 같은 임베디드 시스템에서 문자열 처리를 최적화하는 데 중요한 도구로 여전히 사용되고 있습니다.

알려진 대안 중 하나는 C 스타일 문자열을 사용하여 문자열을 끝나는 null 문자로부터 측정하는 것입니다. 그러나 이 방법은 더 복잡하며 특히 초보자에게는 추천하지 않습니다.

`length() `함수는 내부적으로 문자열의 끝을 나타내는 null 문자를 찾아 문자열의 길이를 계산합니다. 이 정보는 더 고급 프로그래밍에서 유용할 수 있습니다.

## 참고 자료:
다음은 문자열 처리 및 아두이노 프로그래밍에 대한 추가 정보를 제공하는 몇 가지 관련 링크입니다:
* [Arduino String object documentation](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
* [Wikipedia: Null-terminated strings](https://en.wikipedia.org/wiki/Null-terminated_string)