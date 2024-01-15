---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 변환하는 것의 이유는 단순하다. 대문자로 변환하지 않으면 만약에 그 문자열이 일치하거나 비교하고자 하는 대상이 대문자인 경우에는 일치/비교를 실패하게 된다. 알아보기 쉽고 비교 및 일치 확률을 높이기 위해서 문자열을 대문자로 이번 프로그래밍에서는 변환하는 방법에 대해 알아보자.

## 어떻게
아두이노에서 문자열을 대문자로 변환하는 방법은 간단하다. 우선 `upperCase()` 함수를 사용해서 문자열을 대문자로 변환한다. 예를 들어, `hello`라는 문자열을 입력하면 `HELLO`라는 결과가 나오게 된다.
```Arduino
String str = "hello";
str = str.upperCase();
Serial.println(str); // 출력 결과: HELLO
```

전체 코드를 보자면 다음과 같다.
```Arduino
String str = "hello";
str = str.upperCase();
Serial.println(str); // 출력 결과: HELLO
```

## 딥 다이브
`upperCase()` 함수는 문자열을 대문자로 변환해주는 내장 함수이다. 이 함수가 정확히 어떻게 작동하는지 궁금할 수 있다. 대문자로 변환하는 과정은 문자열의 각 문자를 순회하면서 ASCII 코드에서 32를 빼주는 방식으로 이루어진다. 예를 들어, `a`의 ASCII 코드는 97이며, 대문자 `A`의 ASCII 코드는 65이기 때문에 `a`에서 32를 빼주면 `A`가 된다.

또는 `toupper()` 함수를 사용해서 문자열을 대문자로 변환할 수도 있다. 이 함수는 단일 문자에만 적용되기 때문에 반복문을 사용해서 문자열의 모든 문자에 대해 적용해야 한다.

## 참고
- [Arduino 공식 사이트](https://www.arduino.cc/)
- [ASCII 코드표](https://www.asciitable.com/)

## 더 보기
- [Arduino에서 문자열 다루기](https://arduinogetstarted.com/reference/arduino-string)