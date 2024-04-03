---
date: 2024-01-20 17:50:54.906274-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774\uB780 \uBB34\uC5C7\uC774\uACE0\
  \ \uC65C \uC0AC\uC6A9\uD558\uB294\uAC00? \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBCC0\
  \uC218\uB098 \uD45C\uD604\uC2DD\uC758 \uAC12\uC744 \uBB38\uC790\uC5F4 \uC548\uC5D0\
  \ \uC9C1\uC811 \uC0BD\uC785\uD558\uB294 \uAE30\uBC95\uC785\uB2C8\uB2E4. \uCF54\uB4DC\
  \uB97C \uB354 \uBA85\uD655\uD558\uACE0 \uAC04\uACB0\uD558\uAC8C \uB9CC\uB4E4\uAE30\
  \ \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.027161-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774\uB780 \uBB34\uC5C7\uC774\uACE0 \uC65C\
  \ \uC0AC\uC6A9\uD558\uB294\uAC00."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

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
