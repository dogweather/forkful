---
title:                "텍스트 검색 및 대체하기"
html_title:           "Java: 텍스트 검색 및 대체하기"
simple_title:         "텍스트 검색 및 대체하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

사람들이 텍스트를 찾고 대체하는 행위를 하는 이유는 단순히 특정 문자열을 빠르게 찾고 변경하기 위해서입니다.

## 어떻게

먼저, `String` 클래스의 `replace()` 메소드를 사용하여 특정 문자열을 다른 문자열로 대체할 수 있습니다. 예를 들어:

```Java
String str = "Hello World";
String newStr = str.replace("Hello", "Hi");
System.out.println(newStr);
```

출력 결과는 `Hi World`가 될 것입니다.

또는 정규표현식을 사용하여 문자열에서 원하는 패턴을 찾아서 대체할 수 있습니다. 예를 들어:

```Java
String str = "banana";
String newStr = str.replaceAll("a", "o"); // 모든 a를 o로 대체
System.out.println(newStr);
```

출력 결과는 `bonono`가 될 것입니다.

## 심화 분석

`replace()` 메소드는 정확한 문자열을 찾아서 대체하지만, 정규표현식은 더 다양한 패턴을 찾을 수 있습니다. 따라서 정규표현식을 사용하면 더 유연하게 텍스트를 검색하고 대체할 수 있습니다. 또한 `replace()`와 `replaceAll()` 메소드는 대상 문자열을 직접 변경하지 않고 새로운 문자열을 반환하기 때문에 원본 문자열을 보존할 수 있습니다.

## 또 다른 참고자료

- [Java String 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java 정규표현식 튜토리얼](https://docs.oracle.com/javase/tutorial/essential/regex/)