---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 연결은 두 개 이상의 문자열을 하나의 문자열로 합치는 프로세스입니다. 프로그래머는 데이터의 표현을 보다 유연하고 가독성이 좋게 만들기 위해 이를 사용합니다.

## 방법은?

아래는 문자열 연결의 기본적인 사용법입니다:
```Java
String str1 = "Hello,";
String str2 = "World!";
String joinedStr = str1 + str2;
System.out.println(joinedStr); // 출력: Hello,World!
```
Java에서는 `+` 연산자를 사용하여 문자열을 쉽게 연결할 수 있습니다.

## 깊게 알아보기

역사적 측면에서 보자면, 최초의 문자열 연결은 '+' 연산자의 오버로딩으로 제공되었습니다. 그러나 문자열이 많아질수록 성능 이슈가 발생했습니다. 이를 해결하기 위해 `StringBuilder` 클래스가 도입되었습니다.

대안적으로, `StringBuilder` 또는 `StringBuffer`를 사용하여 문자열을 연결할 수 있습니다. 이들은 메모리를 더 효율적으로 사용하며, 복잡한 문자열 연산을 수행하는데 더 유리합니다.

```Java
StringBuilder sb = new StringBuilder();
sb.append("Hello,");
sb.append("World!");
System.out.println(sb.toString()); // 출력: Hello,World!
```
문자열 연결의 세부구현은 JVM에서 처리되며, JVM은 컴파일 시점에서 `+`를 `StringBuilder.append()`로 변환합니다.

## 참고

문자열 연결에 대한 보다 상세한 정보는 다음의 자료를 참조하십시오:

1. Oracle Java 문서: [StringBuilder](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/StringBuilder.html)
2. Java Tutorials: [Manipulating Characters in a String](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)