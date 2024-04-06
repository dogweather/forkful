---
date: 2024-01-20 17:47:44.670279-07:00
description: "How to: (\uBC29\uBC95:) \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C\
  \ \uCC3E\uB294 \uAC83\uC740 Java\uC5D0\uC11C \uB9E4\uC6B0 \uAC04\uB2E8\uD569\uB2C8\
  \uB2E4. `String` \uAC1D\uCCB4\uC758 `.length()` \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\
  \uD558\uBA74 \uB429\uB2C8\uB2E4. \uC774 \uBA54\uC18C\uB4DC\uB294 \uBB38\uC790\uC5F4\
  \uC774 \uC0DD\uC131\uB420 \uB54C \uACC4\uC0B0\uB41C \uAE38\uC774 \uAC12\uC744 \uBC18\
  \uD658\uD569\uB2C8\uB2E4. \uC624\uB798 \uC804\uBD80\uD130 Java\uC5D0 \uC788\uB358\
  \ \uAE30\uB2A5\uC778\uB370, \uBB38\uC790\uC5F4 \uCC98\uB9AC\uC758\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.807164-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294\
  \ \uAC83\uC740 Java\uC5D0\uC11C \uB9E4\uC6B0 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (방법:)
```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "안녕하세요!";
        int length = greeting.length();
        System.out.println("문자열 길이: " + length);
    }
}
```
출력:
```
문자열 길이: 6
```

## Deep Dive (심층 분석)
문자열의 길이를 찾는 것은 Java에서 매우 간단합니다. `String` 객체의 `.length()` 메소드를 사용하면 됩니다. 이 메소드는 문자열이 생성될 때 계산된 길이 값을 반환합니다. 오래 전부터 Java에 있던 기능인데, 문자열 처리의 기본이라고 할 수 있죠.

다른 방법으로 `String.toCharArray()`를 통해 문자 배열을 만들고 배열의 `.length` 속성을 확인할 수도 있습니다. 하지만 이런 방식은 불필요한 배열 생성으로 메모리를 낭비할 수 있으므로 `.length()` 메소드 사용이 권장됩니다.

Java의 문자열은 `UTF-16` 인코딩을 사용합니다. 각 `char`는 2바이트를 차지하며, `.length()`는 `char` 개수를 반환합니다. 유니코드 보충 문자는 두 개의 `char`로 표현되므로 길이를 잘못 계산할 수 있습니다. 이를 처리하기 위해서는 `codePointCount`나 `Character` 클래스의 도움을 받아야 합니다.

## See Also (더 보기)
- [Oracle Java Documentation on Strings](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [The Java™ Tutorials – Characters and Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Unicode Standard](http://www.unicode.org/standard/standard.html)
