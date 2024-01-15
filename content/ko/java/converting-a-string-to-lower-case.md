---
title:                "문자열을 소문자로 변환하기"
html_title:           "Java: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

변수 사용 또는 소프트웨어 성능 최적화 등의 이유로, 대소문자가 있거나 실수로 대소문자를 섞은 문자열을 소문자로 바꾸어야 할 때가 있습니다.

## 어떻게

간단한 Java 코드를 이용하여 대소문자가 혼합된 문자열을 소문자로 바꿀 수 있습니다. 예를 들어, 다음과 같은 코드를 사용할 수 있습니다.

```Java
String str = "HeLlO wOrLd";

// str 변수의 값을 소문자로 바꾸기
str = str.toLowerCase();

// 결과 출력
System.out.println(str);

// 출력 결과: hello world
```

이 코드에서 `toLowerCase()` 메소드는 문자열을 소문자로 바꾸는 역할을 합니다. 만약 대문자를 사용해야 된다면 `toUpperCase()` 메소드를 사용하면 됩니다.

## 깊게 들어가기

Java에서 문자열은 불변(immutable)이기 때문에 `toLowerCase()` 메소드는 새로운 문자열을 반환합니다. 따라서, 이전에 선언한 변수에 할당하지 않으면 원본 문자열은 그대로 유지됩니다. 또한, 이 메소드는 String 클래스의 메소드이기 때문에 다른 클래스에서는 사용할 수 없습니다. 만약 다른 클래스에서도 대소문자를 변환해야 한다면, `toLowerCase()`와 같은 메소드를 다른 클래스에도 추가해야 합니다.

## See Also

- [Java String 인터페이스 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java ToLowerCase() 메소드 예제](https://www.geeksforgeeks.org/java-string-tolowercase-method-with-example/)