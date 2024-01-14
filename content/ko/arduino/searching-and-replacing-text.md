---
title:                "Arduino: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 교체하는 것은 아두이노 프로그래밍에서 자주 사용되는 기술 중 하나입니다. 이를 통해 우리는 특정 단어나 구문을 찾고 원하는 내용으로 변경할 수 있습니다. 이 기능을 사용하면 코드를 더 효율적으로 작성할 수 있고, 오류를 방지할 수 있으며, 원하는 결과를 더 빠르게 얻을 수 있습니다.

## 하는 법

우선, 우리는 `String` 변수를 사용하여 원하는 텍스트를 저장할 수 있습니다. 예를 들어, `myText = "Hello World!"`라는 변수를 선언하고, `println(myText)`를 사용하면 "Hello World!"가 시리얼 모니터에 출력됩니다.

하지만 우리가 원하는 것은 특정 단어나 구문을 찾아서 원하는 내용으로 바꾸는 것입니다. 이를 위해서는 `replace()` 함수를 사용해야 합니다. 함수의 구조는 다음과 같습니다.

```
myText.replace(찾고자 하는 내용, 바꾸고자 하는 내용);
```

예를 들어, `myText`의 값을 "Hello World!"에서 "Goodbye World!"로 바꾸고 싶다면, 다음과 같이 코드를 작성할 수 있습니다.

```
myText.replace("Hello", "Goodbye");
```

`replace()` 함수는 이전 값만 바꿔주는 것이 아니라, 모든 값을 바꾸기 때문에 조심해야 합니다. 예를 들어, "Hello hello hello"라는 문자열이 있다면, `replace("hello", "goodbye")`를 사용하면 "goodbye goodbye goodbye"가 됩니다.

## 깊숙한 이해

`replace()` 함수는 `String` 변수뿐만 아니라, `Serial` 모니터에 출력한 값 등 다른 문자열에서도 사용할 수 있습니다. 또한, `replace()` 함수의 대소문자를 구분하지 않으므로, 예를 들어 "Hello"를 찾더라도 "hello"도 동시에 바꾸게 됩니다.

또한 `replace()` 함수는 원하는 내용이 포함된 모든 문자열을 변경해버리므로, 불필요한 변경이 발생할 수도 있습니다. 이를 방지하기 위해서는 `replace()` 함수 대신 `replaceFirst()` 함수를 사용할 수 있습니다. 이 함수는 첫 번째로 찾은 내용만 변경해주기 때문에 원하는 결과를 얻을 수 있습니다.

## 또 다른 참고자료

- [The Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [How to Replace Text in a String](https://www.arduino.cc/en/Tutorial/StringReplaceCommand)
- [Exploring the String Library](https://www.arduino.cc/en/Tutorial/StringObject), 예제 코드와 함께 더 깊은 이해를 돕는 튜토리얼