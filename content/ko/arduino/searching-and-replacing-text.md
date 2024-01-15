---
title:                "텍스트 검색 및 대체"
html_title:           "Arduino: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
문자열을 검색하고 바꾸는 방법을 배우는 이유는 더 효율적인 코딩을 위해서입니다.

## 어떻게
많은 텍스트 처리 작업 중 일반적인 작업 중 하나는 특정 문자열을 찾고 해당 문자열을 새로운 문자열로 대체하는 것입니다. 이를 위해 Arduino에서 제공하는 `replace()` 함수를 사용할 수 있습니다. 아래 코드는 `replace()` 함수를 사용하는 간단한 예제입니다.
```
ArduinoString myString = "Hello, world!";
myString.replace("world", "Arduino");
Serial.println(myString); // 출력 결과: Hello, Arduino!
```

`replace()` 함수는 원하는 문자열이 여러 번 나와도 모두 찾아서 바꿀 수 있습니다. 또한 대소문자를 구분하지 않고 문자열을 찾아서 바꿀 수도 있습니다. 예를 들어, 아래 코드를 실행하면 `Hello`를 `Arduino`로 바꿔주는 예제 코드입니다.
```
ArduinoString myString = "Hello, world!";
myString.replace("hello", "Arduino", true);
Serial.println(myString); // 출력 결과: Arduino, world!
```

## 깊이 파고들기
`replace()` 함수는 두 개의 매개변수를 필요로 합니다. 첫 번째 매개변수는 찾고자 하는 문자열이고, 두 번째 매개변수는 대체할 문자열입니다. 또한, 세 번째 매개변수는 대소문자를 구분할지 여부를 결정하는 불리언 값입니다. 기본값은 `false`이며, `true`로 설정하면 대소문자를 구분하지 않게 됩니다.

`replace()` 함수 외에도 Arduino에서는 `replaceAll()` 함수를 사용하여 문자열을 전체적으로 한 번에 바꿀 수도 있습니다. 이 함수는 `replace()` 함수와 달리 대소문자를 구분하지 않으며, 일치하는 모든 문자열을 찾아서 한 번에 바꿔줍니다.

또한, Arduino에서는 정규식을 사용하여 문자열을 검색하고 바꾸는 방법도 제공합니다. 이를 위해 `Regex` 라이브러리를 사용하며, `find()`와 `replace()` 함수를 사용하여 정규식을 처리할 수 있습니다. 하지만 정규식은 좀 더 복잡한 패턴을 찾을 때 사용하며, 간단한 문자열 검색과 교체에는 `replace()` 함수를 사용하는 것이 적합합니다.

## 더 알아보기
- [Official Arduino Reference - replace()](https://www.arduino.cc/reference/ko/language/variables/data-types/string/functions/replace/)
- [Official Arduino Reference - find()](https://www.arduino.cc/reference/ko/libraries/regex/find/)
- [Official Arduino Reference - replaceAll()](https://www.arduino.cc/reference/ko/libraries/regex/replaceall/)