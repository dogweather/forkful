---
title:                "Java: 문자열을 소문자로 변환하기"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 참여하는 이유는 프로그래밍에서 매우 일반적인 작업이기 때문입니다. 대부분의 프로그램은 사용자에게 입력을 받고, 이를 처리한 후 소문자 형태로 결과를 제공합니다. 따라서 문자열을 소문자로 변환하는 것은 다양한 프로그래밍 과정에서 필수적입니다.

## 어떻게
문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다. Java 프로그래밍에서는 `toLowerCase()` 메소드를 사용하여 문자열을 소문자로 변환할 수 있습니다. 아래 코드 블록을 참고해주세요.

```Java
String str = "HELLO WORLD";
String lowercaseStr = str.toLowerCase();
System.out.println(lowercaseStr);
```

위 코드를 실행하면 "hello world"가 출력됩니다.

## 깊이 파고들기
문자열을 소문자로 변환하는 메소드 `toLowerCase()`의 내부 동작 원리를 알아보겠습니다. `toLowerCase()`는 문자열의 각 문자를 하나씩 검사하고, 대문자 알파벳의 경우에만 ASCII 값에서 32를 빼서 소문자로 변환합니다. 그 이외의 문자는 그대로 유지됩니다. 이 과정을 모든 문자에서 반복하여 문자열 전체를 소문자로 변환하는 것입니다.

## 다른 자료

- [Java String toLowerCase() 메소드 정보](https://www.w3schools.com/java/ref_string_tolowercase.asp)
- [ASCII 코드표](https://ko.wikipedia.org/wiki/ASCII)