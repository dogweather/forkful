---
title:                "Java: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는데 관심을 가지는 이유는 무엇일까요? 일반적으로 문자열을 소문자로 변환하면 입력 문자열을 정규화하고 비교 및 검색 작업을 더 쉽게 수행할 수 있습니다.

## 해는

```Java
String str = "Hello World";
String lowerStr = str.toLowerCase();
System.out.println(lowerStr);
```
#### 결과: hello world

문자열을 소문자로 변환하는 방법은 간단합니다. Java의 `toLowerCase()` 함수를 사용하면 입력된 문자열을 소문자로 변환할 수 있습니다. 이 함수는 새로운 문자열을 반환하기 때문에 해당 문자열에 직접 영향을 미치지 않습니다.

```Java
String str = "JAVA IS FUN";
String lowerStr = str.toLowerCase();
System.out.println(lowerStr);
```

#### 결과: java is fun

또한 이러한 변환은 대소문자를 구분하지 않는 검색 작업에 매우 유용합니다. 만약 입력된 문자열이 `java is fun`이라면 `toLowerCase()` 함수를 사용해서 모든 문자열을 소문자로 변환하고 검색에서 중복되는 실수를 줄일 수 있습니다.

## 딥 다이브

이미 살펴본 것처럼 `toLowerCase()` 함수는 Java에서 문자열을 소문자로 변환하는 가장 간단한 방법입니다. 하지만 이 함수는 Locale(로케일)을 고려하지 않기 때문에 언어에 따라 다른 결과를 산출할 수 있습니다.

예를 들어, 터키어에는 `ı`, `I`와 같이 우리가 일반적으로 알고 있는 `i`, `I`가 아닌 문자가 있습니다. 따라서 터키어에서 `toLowerCase()` 함수를 사용하면 이러한 문자들이 다른 문자로 변환됩니다.

이에 대한 해결책은 `toUpperCase(Locale)` 함수를 사용하는 것입니다. 이 함수는 로케일을 매개변수로 받아 해당 언어에 맞는 문자를 반환합니다. 따라서 터키어에서는 `toLowerCase(new Locale("tr"))`와 같이 사용해야 합니다.

다른 주의할 점은 `toLowerCase()` 함수가 영어를 기준으로 만들어진 함수라는 것입니다. 다른 언어에서는 이 함수를 사용하지 않는 것이 좋습니다. 대신 해당 언어에 맞는 변환 함수를 사용하는 것이 좋습니다.

## 참고

- [Java String 관련 함수들](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Locale 관련 함수들](https://docs.oracle.com/javase/8/docs/api/index.html?java/util/Locale.html)
- [Java ToLowerCase() function](https://www.tutorialspoint.com/java/java_string_tolowercase.htm)
- [Java ToUpperCase() function](https://www.tutorialspoint.com/java/java_string_touppercase.htm)
- [Java Locale 에서 특정 로케일을 사용하는 방법](https://www.baeldung.com/java-locale)