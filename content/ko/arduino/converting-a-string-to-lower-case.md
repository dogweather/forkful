---
title:                "대문자를 소문자로 변환하기"
html_title:           "Arduino: 대문자를 소문자로 변환하기"
simple_title:         "대문자를 소문자로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 하는가?
문자열을 소문자로 변환하는 것은 프로그래머가 자주 하는 작업 중 하나입니다. 이를 통해 입력된 모든 문자를 연산하기 쉽게 만들어주기 때문입니다. 예를 들어, 사용자로부터 입력된 문자열을 대문자와 소문자를 구분하지 않고 처리하고자 할 때 이 기능을 사용하면 효율적으로 코드를 작성할 수 있습니다.

## 어떻게 하나요?
우선, 변환하고자 하는 문자열을 저장할 변수를 선언합니다. 그 다음, ```lowerCase()``` 함수를 사용하여 해당 변수의 값을 소문자로 변환합니다. 최종적으로, 변환된 문자열을 출력하거나 다른 변수에 저장할 수 있습니다.

```Arduino
String str = "Hello World";
str = str.toLowerCase();
Serial.println(str); // 출력: hello world
```

## 더 깊게 알아보기
사실, 문자열을 소문자로 변환하는 기능은 오래 전부터 사용되어왔습니다. 예를 들어, 1980년대부터 사용되던 C 프로그래밍 언어에서도 이 기능을 지원하고 있었습니다. 또한, 언어마다 다르지만 보통 대문자와 소문자를 구분하지 않고 문자열을 처리하는 기능을 제공하기도 합니다.

이러한 기능을 구현하는 방법은 여러 가지가 있지만, 대부분 문자열을 각 문자의 아스키 코드로 변환한 후 해당 값을 조작하는 방식을 채택합니다.

## 관련 자료
- [String 클래스 공식 문서](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [ASCII 코드](https://en.wikipedia.org/wiki/ASCII)