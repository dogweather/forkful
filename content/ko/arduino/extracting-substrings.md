---
title:                "Arduino: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

우리가 일상적으로 사용하는 문자열에서 일부분을 추출할 필요가 있을 수 있습니다. Arduino에서 substring을 추출하는 방법을 배우면, 더 다양한 프로젝트를 만들 수 있고 다양한 기능을 추가할 수 있습니다.

## 어떻게 하나요

Arduino에서 substring을 추출하는 방법은 매우 간단합니다. 먼저, 추출하고 싶은 문자열을 지정하고 그 문자열의 시작과 끝 인덱스를 정해줍니다. 그리고 Arduino의 내장 함수 중 하나인 substring() 함수를 사용하면 됩니다. 아래는 코드 예시와 함께 설명하겠습니다.

```Arduino
String str = "Hello World!"; // 추출하고 싶은 문자열
String sub1 = str.substring(0, 5); // 시작 인덱스 0부터 끝 인덱스 4까지 추출합니다.
String sub2 = str.substring(6); // 6부터 끝까지 추출합니다.

Serial.println(sub1); // 출력: "Hello"
Serial.println(sub2); // 출력: "World!"
```

위의 코드에서는 `substring()` 함수를 사용하여 간단히 문자열을 추출할 수 있습니다. 또한, 인덱스 값을 이용하여 원하는 부분만 추출할 수 있기 때문에 다양한 활용이 가능합니다. 

## 딥 다이브

Arduino에서 substring을 추출하는 또 다른 방법으로는 `indexOf()` 함수를 사용하는 방법이 있습니다. 이 함수는 원하는 문자나 문자열이 어느 위치에 있는지 찾아주는 함수입니다. 예를 들어, 아래와 같은 코드를 작성하면 "World"의 시작 인덱스를 알 수 있습니다.

```Arduino
String str = "Hello World!";
int index = str.indexOf("World"); // index = 6

```

이렇게 얻은 인덱스 값을 `substring()` 함수에 넣어주면 "World"만 추출할 수 있습니다.

또 한 가지 중요한 점은 Arduino에서 `substring()` 함수를 사용하기 위해서는 `String` 라이브러리를 추가해야 한다는 점입니다. 아두이노 IDE에서 `Sketch` 메뉴, `Include Library` 메뉴로 들어가면 `String` 라이브러리를 추가할 수 있습니다.

## 참고

만약 Arduino를 처음 사용하거나 `substring()` 함수를 사용하는 것이 낯설다면, 아래의 링크들을 참고하시면 좋을 것 같습니다.

- [Arduino String 라이브러리 공식 문서](https://www.arduino.cc/reference/ko/language/variables/data-types/string/)
- [Arduino 튜토리얼: 문자열 다루기](https://www.arduino.cc/en/Tutorial/StringConstructors)

## 더 알아보기

- [Arduino String 라이브러리 추가 방법 정리](https://m.blog.naver.com/wooh_hyun/221216871334)
- [String과 char의 차이점 이해하기](https://m.blog.naver.com/PostView.nhn?blogId=showstopper&logNo=220316169113&proxyReferer=https%3A%2F%2Fwww.google.com%2F)