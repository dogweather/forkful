---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?

문자열을 소문자로 변환한다는 것은 모든 대문자를 소문자로 바꾸는 것을 의미합니다. 프로그래머는 이것을 일관성 확보와 데이터 비교를 용이하게 하기 위해 실행합니다.

## 어떻게 합니까:
```Arduino 
String data = "Hello World";
data.toLowerCase();
Serial.println(data); // 출력: hello world
``` 
위의 코드는 문자열 "Hello World"를 모두 소문자로 변환하고, 
알려진 디바이스에 출력합니다.

## 깊게 알아보기:
사실, 문자열을 소문자로 변환하는 것은 매우 오래된 컨셉입니다. 현대의 대부분 프로그래밍 언어에서 기본적으로 제공되는 기능 중 하나입니다. 단순 한 함수 호출로 쉽게 구현할 수 있습니다. 다른 대안적 방법으로 ASCII 코드를 이용하여 직접 구현하는 방법이 있으나, Arduino에서 기본적으로 제공하는 toLowerCase() 함수를 이용하는 것이 편리합니다.
toLowerCase() 함수는 문자열 내의 모든 대문자를 ASCII 코드를 이용하여 소문자로 변환하며, 변환 후의 문자열을 원래의 변수에 다시 할당합니다.

## 참고:

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCII Table](https://www.asciitable.com/)
- [How to use strings in Arduino programming](https://create.arduino.cc/projecthub/harshmangukiya/how-to-use-strings-in-arduino-programming-2d479b)