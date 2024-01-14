---
title:    "Java: 텍스트 검색 및 대체"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 왜

문자열을 검색하고 대체하는 작업은 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 이를 통해 쉽게 특정한 패턴을 가진 문자열을 찾을 수 있고, 필요한 변경을 쉽게 할 수 있습니다.

# 사용 방법

초보자들도 쉽게 따라 할 수 있는 Java에서 문자열 검색 및 대체하는 방법을 알아보겠습니다. 문자열 검색은 문자열을 검색할 때 사용되는 정규 표현식을 매개 변수로 받아 호출합니다. 예를 들어, "JAVA"라는 문자열을 "JAVASCRIPT"로 대체하고 싶다면 다음과 같은 코드를 작성할 수 있습니다.

```java
String orgString = "JAVA is a programming language.";
String newString = orgString.replace("JAVA", "JAVASCRIPT");
System.out.println(newString);
```

결과는 "JAVASCRIPT is a programming language."가 됩니다.

또 다른 예시로는 특정한 패턴을 가진 문자열의 모든 공백을 제거하고 싶은 경우, 다음과 같이 작성할 수 있습니다.

```java
String orgString = "Java is a programming language.";
String newString = orgString.replaceAll("\\s+", "");
System.out.println(newString);
```

출력 결과는 "Javaisaprogramminglanguage."가 됩니다.

# 깊게 파고들기

문자열 검색 및 대체는 정규 표현식과 함께 자주 사용되며, 정규 표현식을 사용하면 좀 더 유연하게 문자열을 검색하고 대체할 수 있습니다. 또한, replace()와 replaceAll() 메소드 외에도 StringBuilder 클래스를 사용하여 문자열을 쉽게 변경할 수 있습니다.

또한, 문제가 발생할 수 있는 경우도 있습니다. 예를 들어, replaceAll() 메소드는 정규 표현식을 매개 변수로 받기 때문에 잘못된 정규 표현식을 사용하면 예외가 발생할 수 있습니다. 따라서 정규 표현식을 사용할 때에는 신중하게 검토하고 테스트하는 것이 중요합니다.

# 참고

- https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-java.lang.CharSequence-java.lang.CharSequence-
- https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- https://dzone.com/articles/searching-replacing-and-building-with-java-strings
- https://www.baeldung.com/java-replace-all