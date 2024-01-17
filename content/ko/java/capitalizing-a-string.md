---
title:                "문자열 대문자로 변환하기"
html_title:           "Java: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무슨 말이야?
문자열의 첫 번째 문자를 대문자로 만드는 것을 문자열 대문자화라고 합니다. 프로그래머들은 주로 입력된 문자열의 첫 번째 문자를 대문자로 바꾸는 것을 선호하는데, 이는 엄격한 코딩 규칙을 준수하고 문자열을 보다 더 보기 좋게 만들기 위해서입니다.

## 어떻게 하지?
자바 코드 블록 안에 코딩 예제와 예상 출력을 포함하여 문자열 대문자화를 어떻게 할 수 있는지 알아보겠습니다.

```Java
// 문자열 대문자화 예제
String input = "hello world";
String output = input.substring(0, 1).toUpperCase() + input.substring(1);

System.out.println(output); // 출력: Hello world
```

## 깊게 파헤쳐보기
1. 역사적 배경: 초기 프로그래밍에서는 변수 이름을 모두 대문자로 작성하는 것이 일반적이었지만, 현재는 소문자와 대문자를 구분하는 코드 컨벤션에 따라 대문자화를 선호합니다.
2. 대안: 문자열 대문자화에는 다양한 방법이 있지만 가장 일반적인 방법은 substring() 메소드와 toUpperCase() 메소드를 사용하는 것입니다.
3. 구현 세부사항: 입력된 문자열을 잘라 첫 번째 문자를 대문자로 변환하고 나머지 문자열을 원래의 상태로 유지하는 방식으로 구현할 수 있습니다.

## 관련 자료
문자열에 대한 더 많은 정보를 알고 싶다면 아래 링크를 참고하세요.
- [Java Substring](https://www.w3schools.com/java/java_strings_substring.asp)
- [Java String toUpperCase() Method](https://www.w3schools.com/java/ref_string_touppercase.asp)
- [Java Coding Conventions](https://en.wikipedia.org/wiki/Naming_convention_(programming)#Java)