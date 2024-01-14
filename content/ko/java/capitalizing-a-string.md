---
title:                "Java: 문자열 대문자로 변환하기"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열을 대문자로 바꾸는 작업을 수행하는 이유는 다양합니다. 예를 들어, 사용자로부터 입력받은 데이터를 일관성있게 표현하거나, 특정 키워드가 들어간 문자열을 찾아내는 등의 목적으로 사용할 수 있습니다.

## 어떻게 해야 할까요?

자바에서 문자열을 대문자로 변환하는 방법에는 여러가지가 있지만, 그 중에서도 가장 간편하고 사용하기 쉬운 방법은 `toUpperCase()` 메소드를 사용하는 것입니다. 이 메소드는 문자열의 모든 문자를 대문자로 변환해주는 역할을 합니다. 예를 들어, 다음과 같이 코드를 작성하면 입력받은 문자열을 대문자로 변환할 수 있습니다.

```Java
// 사용자로부터 입력받은 문자열
String input = "Hello world!";

// 대문자로 변환
String capitalized = input.toUpperCase();

// 변환된 문자열 출력
System.out.println(capitalized);

// 결과: HELLO WORLD!
```

## 딥 다이브

자바에서 문자열을 대문자로 변환하는 방법에는 `toUpperCase()` 메소드 말고도, `StringBuffer`나 `StringBuilder` 클래스를 활용하는 방법도 있습니다. 또한, 정규표현식을 사용하여 특정 패턴의 문자열만 대문자로 변환할 수도 있습니다. 이러한 다양한 방법을 익히면 다양한 상황에서 문자열을 대문자로 변환하는 작업을 보다 쉽고 효율적으로 수행할 수 있습니다.

# 참고자료

- [Oracle Java Docs에서 toUpperCase() 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [TutorialsPoint에서 Java에서 대문자로 변환하기](https://www.tutorialspoint.com/java/java_string_touppercase.htm)
- [Baeldung 블로그에서 Java에서 대소문자 변환하기](https://www.baeldung.com/java-change-string-case)