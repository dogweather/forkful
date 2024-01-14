---
title:                "Java: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜

문자열을 결합하는 것은 자바 프로그래밍에서 매우 중요합니다. 이를 통해 여러 문자열을 하나로 조합할 수 있으며, 이를 통해 더 복잡한 작업을 수행할 수 있습니다.

# 어떻게 하나요

```Java
String firstName = "John";
String lastName = "Smith";
String fullName = firstName + " " + lastName;
System.out.println(fullName);
```

위의 예시 코드에서는 먼저 성과 이름을 각각 변수로 지정하고, 마지막으로 그 두 변수를 결합하여 하나의 문자열을 만들고 출력합니다. 이처럼 '+' 연산자를 사용하면 간단하게 문자열을 결합할 수 있습니다.

출력: John Smith

# 깊이 파고들기

문자열을 결합하는 방법은 여러 가지가 있지만, 가장 흔한 방법은 '+' 연산자를 사용하는 것입니다. 이 연산자는 문자열과 변수, 숫자 등을 결합할 수 있으며, 결합한 결과는 새로운 문자열이 됩니다.

그 외에도 StringBuilder나 StringBuffer 클래스를 사용하여 문자열을 동적으로 변경하고 결합하는 방법이 있습니다. 이를 통해 메모리를 더 효율적으로 관리할 수 있으며, 문자열 결합의 성능을 높일 수 있습니다.

# 또 다른 정보

- [Java String concatenation](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [StringBuilder vs. StringBuffer in Java](https://www.baeldung.com/java-string-builder-vs-string-buffer)
- [Java Operators](https://www.w3schools.com/java/java_operators.asp)