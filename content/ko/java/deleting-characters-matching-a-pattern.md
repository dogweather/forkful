---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Java: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

패턴을 일치하는 문자를 삭제하는 것은 말 그대로 패턴과 일치하는 문자를 찾아서 삭제하는 것을 의미합니다. 이는 특정한 작업을 수행하기 위해서는 필요하지 않은 문자를 제거하는데 사용됩니다.

프로그래머들이 이를 하는 이유는 보다 깨끗하고 효율적인 코드를 작성하기 위해서입니다. 불필요한 문자를 삭제함으로써 코드의 가독성을 높이고, 작업을 더욱 빠르고 효율적으로 수행할 수 있게 됩니다.

## 어떻게:

```Java
// 예제 1: 특정 패턴을 일치하는 문자 삭제하기
String str = "Hello, World!";
str = str.replaceAll("[eo]", ""); // "eo" 패턴에 일치하는 문자 삭제
System.out.println(str); // 출력: Hll, Wrld!

// 예제 2: 정규식을 이용한 패턴 삭제
String str = "Java is awesome!";
str = str.replaceAll("[a-z]", ""); // 알파벳 소문자를 모두 삭제
System.out.println(str); // 출력: J

// 예제 3: 특정 단어 삭제
String str = "Today is a great day!";
str = str.replaceAll("great", ""); // "great" 단어 삭제
System.out.println(str); // 출력: Today is a day!
```

## 깊게 파고들기:

(1) 패턴을 일치하는 문자를 삭제하는 작업은 오래 전부터 사용되었습니다. 그리고 오늘날에도 여전히 많은 개발자들이 이를 사용합니다. (2) 패턴을 일치하는 문자를 삭제하는 대안으로는 다른 문자로 대체하는 방법이 있습니다. 이는 꼭 문자를 삭제해야 할 필요가 없을 때 매우 유용합니다. (3) 구현 세부 사항을 보면 문자열을 조작하기 위해 정규식(regular expression)이 사용된다는 것을 알 수 있습니다. 정규식은 패턴을 검색, 대체 및 분할하기 위해 사용되는 강력한 도구입니다.

## 관련 자료 보기:

- [Java String 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java 중간 수준 프로그래밍 언어(Pattern matching)](https://www.javatpoint.com/java-pattern-matching)