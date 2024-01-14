---
title:                "Java: 문자열의 길이 찾기"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열 길이를 찾는 것에 참여하는 이유는 무엇일까요? 이 질문은 많은 프로그래머들이 자주 묻는 질문 중 하나입니다. 문자열의 길이를 알 수 있다면, 우리는 문자열을 다루는 데 더 많은 옵션을 가질 수 있고, 프로그램의 제어 흐름을 유연하게 조작할 수 있게 됩니다.

## 어떻게?

그렇다면, 자바에서 문자열 길이를 찾는 방법은 무엇일까요? 우선, 문자열의 길이를 알고 싶은 변수를 선언합니다. 그리고 `length()` 메소드를 사용하여 변수에 할당된 문자열의 길이를 가져옵니다. 아래는 `length()` 메소드의 예시 코드와 출력 결과입니다.

```Java
String str = "안녕하세요!";
System.out.println(str.length());
```
**출력 결과: 6**

위의 예시 코드에서 `length()` 메소드는 문자열의 길이를 반환하는 것을 볼 수 있습니다. 문자열의 길이는 공백과 특수 문자를 포함하여 계산됩니다.

## 딥 다이브

문자열 길이를 찾는 것은 매우 간단해 보입니다. 하지만 실제로는 어떻게 동작하는 걸까요? 자바에서 `length()` 메소드를 사용하여 문자열 길이를 반환하는 이유는 바로 문자열 클래스에서 `length` 변수를 통해 문자열의 길이를 저장하고 있기 때문입니다. 이 변수는 문자열이 생성될 때 자동으로 초기화되며, `length()` 메소드가 호출될 때마다 반환됩니다. 따라서 우리는 `length()` 메소드를 호출하여 문자열의 길이를 쉽게 알아낼 수 있습니다.

## 참고자료

- [Java String length() 메소드](https://www.w3schools.com/java/ref_string_length.asp)
- [Java Reference - String Class](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)