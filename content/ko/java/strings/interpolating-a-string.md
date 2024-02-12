---
title:                "문자열 보간하기"
aliases:
- ko/java/interpolating-a-string.md
date:                  2024-01-20T17:50:54.906274-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
문자열 보간이란 무엇이고 왜 사용하는가?
문자열 보간은 변수나 표현식의 값을 문자열 안에 직접 삽입하는 기법입니다. 코드를 더 명확하고 간결하게 만들기 위해 사용합니다.

## How to:
자바에서의 문자열 보간 방법
```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "Kim";
        int age = 25;

        String greeting = String.format("Hello, %s! You are %d years old.", name, age);
        System.out.println(greeting);
    }
}
```
출력:
```
Hello, Kim! You are 25 years old.
```

## Deep Dive:
문자열 보간에 대한 깊은 이해

보간은 오래전부터 프로그래밍에서 사용된 개념입니다. Java에서는 `%s`, `%d` 같은 형식 지정자를 `String.format()` 메서드에 쓰는 방식으로 보간 기능을 사용합니다. 최신 언어들은 `${variable}` 같은 표현식을 직접 쓸 수 있지만, Java에선 아직 이런 단순한 문법이 없습니다.

다른 방법:
- `+` 연산자로 문자열에 변수 연결하기.
- `StringBuilder`나 `StringBuffer` 클래스 사용하기.

구현 세부 사항:
- 내부적으로 `String.format()`은 `Formatter` 클래스를 사용합니다.
- 성능이 중요하다면 `+` 연산자 대신 `StringBuilder`를 사용하는 것을 고려해야 합니다, 특히 반복문 안에서 문자열을 많이 다룰 때.

## See Also:
관련 링크:

- 공식 Java 문서의 String.format: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...)
- Formatter 클래스: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Formatter.html
- StringBuilder 클래스에 대한 튜토리얼: https://docs.oracle.com/javase/tutorial/java/data/buffers.html
