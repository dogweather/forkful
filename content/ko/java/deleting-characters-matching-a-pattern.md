---
title:    "Java: 패턴과 일치하는 문자 삭제하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 일치하는 패턴의 문자를 삭제하는 것이 왜 중요한지 궁금하신가요? 이 포스트에서는 이 작업이 왜 유용하고 필요한지에 대해 자세히 알아보겠습니다.

## 어떻게 하나요?

먼저, 문자열을 입력받고 원하는 패턴을 정의합니다.

```Java
String input = "Like apples and oranges, I like to eat both.";
String pattern = "like";
```

그다음, 정규식을 사용하여 패턴을 찾고 그것을 공백으로 대체합니다.

```Java
String output = input.replaceAll(pattern, "");
System.out.println(output);
```

결과는 다음과 같습니다.

```
Apples and oranges, I to eat both.
```

## 더 깊게

문자열에서 일치하는 패턴의 문자를 삭제하는 것은 문자열 처리에 매우 유용합니다. 예를 들어, 사용자로부터 입력받은 이메일 주소에서 "@gmail.com" 도메인을 삭제하여 사용자의 이메일을 가릴 수 있습니다. 또는 특정 단어를 검열하고자 할 때에도 유용합니다. 더욱이, 정규식을 사용하면 더 다양한 패턴을 검색하고 변경할 수 있습니다.

## 또 다른 참조

- [Java 정규식 패턴 예제](https://www.geeksforgeeks.org/regular-expression-in-java/)
- [Java String 클래스 문서](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
- [Baeldung 블로그의 Java 정규식 튜토리얼](https://www.baeldung.com/regular-expressions-java)