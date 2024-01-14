---
title:                "Java: 텍스트 검색 및 대체"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

문자열 검색 및 대체를 수행하는 이유는 개발자들이 쉽고 효율적으로 특정 문자열을 수정하고 조작하기 위해서입니다.

## 하는 법

```Java
// 예제 코드 1: 단일 문자열 대체
String text = "안녕하세요, 저는 Java 프로그래머입니다.";

// replace 메소드를 사용하여 "Java"를 "Kotlin"으로 대체합니다.
String newText = text.replace("Java", "Kotlin");

// 새로운 문자열 출력
System.out.println(newText); 
// 결과: 안녕하세요, 저는 Kotlin 프로그래머입니다.

// 예제 코드 2: 여러 문자열 대체
String text = "나는 사과, 배, 바나나를 좋아합니다.";

// replaceAll 메소드를 사용하여 "사과, 배, 바나나"를 "딸기, 포도, 수박"으로 대체합니다.
String newText = text.replaceAll("사과|배|바나나", "딸기|포도|수박");

// 새로운 문자열 출력
System.out.println(newText); 
// 결과: 나는 딸기, 포도, 수박을 좋아합니다. 
```

## 깊이 파고들기

문자열 검색 및 대체는 정규식을 이용하여 더욱 다양하고 복잡한 패턴을 검색하고 대체할 수 있습니다. 또한 마지막으로 일치하는 항목만 대체하거나, 대체 결과를 쉽게 반복할 수 있는 기능도 제공됩니다. 또한 유니코드 문자들도 대체가 가능합니다.

## 참고 자료

- [Java String replace() 메소드 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-java.lang.CharSequence-java.lang.CharSequence-)
- [Java String replaceAll() 메소드 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Regular Expressions in Java](https://www.tutorialspoint.com/java/java_regular_expressions.htm)