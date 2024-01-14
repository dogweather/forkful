---
title:                "Java: 그 정규식 사용하기"
simple_title:         "그 정규식 사용하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규식(Regular Expressions)을 사용하는 이유는 간단합니다. 정규식은 문자열에서 원하는 패턴을 찾고 추출하는 뛰어난 도구입니다. 예를 들어, 이메일 주소와 전화번호를 입력받아서 형식에 맞는지를 확인하려면 정규식을 사용하면 훨씬 더 효율적으로 처리할 수 있습니다.

## 사용 방법

정규식을 사용하기 위해서는 우선 `java.util.regex` 패키지를 import해야 합니다. 그 다음, `Pattern` 클래스를 사용하여 원하는 패턴을 지정하고, `Matcher` 클래스를 사용하여 입력받은 문자열에서 패턴을 찾아내는 작업을 할 수 있습니다. 예를 들어, 주민등록번호가 `123456-1234567` 형식을 갖고 있는지를 확인하는 코드는 다음과 같습니다.
```java
import java.util.regex.*;

String pattern = "\\d{6}-\\d{7}"; // 주민등록번호 패턴
String input = "123456-1234567"; // 입력받은 주민등록번호

Pattern p = Pattern.compile(pattern); // 패턴 지정
Matcher m = p.matcher(input); // 입력받은 문자열과 패턴을 비교하는 Matcher 객체 생성

if (m.matches()) {
	System.out.println("주민등록번호 형식에 맞습니다.");
} else {
	System.out.println("주민등록번호 형식이 아닙니다.");
}
```
위 코드를 실행하면 "주민등록번호 형식에 맞습니다."라는 결과가 출력됩니다.

## 깊이 파고들기

정규식에는 다양한 메타 문자들이 존재하며, 이를 활용하여 더 복잡한 패턴을 찾아낼 수 있습니다. 예를 들어, `.`은 어떤 문자 하나를 대체하며, `*`은 이전에 올 문자가 0개 또는 그 이상 반복되는 것을 의미합니다. 이런 메타 문자를 조합하여 이메일 주소가 양식에 맞는지를 검사하는 코드를 작성할 수 있습니다.
```java
String pattern = "\\w+@\\w+\\.[a-z]+"; // 이메일 주소 패턴
String input = "example123@mail.com"; // 입력받은 이메일 주소

Pattern p = Pattern.compile(pattern); // 패턴 지정
Matcher m = p.matcher(input); // 입력받은 문자열과 패턴을 비교하는 Matcher 객체 생성

if (m.matches()) {
	System.out.println("올바른 이메일 주소 형식입니다.");
} else {
	System.out.println("올바른 이메일 주소 형식이 아닙니다.");
}
```
위 코드를 실행하면 "올바른 이메일 주소 형식입니다."라는 결과가 출력됩니다.

## [관련 자료](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)

- [Java 정규식 패키지 문서](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html