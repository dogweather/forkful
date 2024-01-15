---
title:                "정규식 사용하기"
html_title:           "Java: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규 표현식을 사용해야 하는가?

정규 표현식은 텍스트에서 특정한 패턴을 찾는 데 매우 유용합니다. 이를 사용하면 강력한 문자열 조작 기능을 제공하여 코드를 더 간결하고 효율적으로 만들 수 있습니다.

## 사용 방법

**1. 패턴 매칭**

정규 표현식을 사용하여 특정 패턴에 일치하는 문자열을 찾을 수 있습니다. 예를 들어, 다음 코드는 텍스트에서 "Java"라는 단어를 찾아 출력합니다.

```Java
Pattern pattern = Pattern.compile("Java"); // 패턴 생성
Matcher matcher = pattern.matcher("I love Java programming!"); // 텍스트와 매칭할 매처 생성
if (matcher.find()) { // 일치하는 문자열이 있는지 확인
  System.out.println(matcher.group()); // 일치하는 문자열 출력
}
```

**2. 패턴 분할**

정규 표현식을 사용하면 텍스트를 특정 패턴으로 분할할 수도 있습니다. 예를 들어, 다음 코드는 쉼표로 구분된 문자열을 분할하여 배열로 저장합니다.

```Java
String str = "apple, banana, orange";
String[] fruits = str.split(","); // 쉼표로 분할
for(String fruit : fruits) {
  System.out.println(fruit.trim()); // 공백 제거하고 출력
}
```

**3. 패턴 추출**

정규 표현식을 사용하여 텍스트에서 특정 패턴을 추출할 수도 있습니다. 예를 들어, 다음 코드는 전화번호에서 숫자만 추출하여 출력합니다.

```Java
Pattern pattern = Pattern.compile("\\d+"); // 숫자 패턴 생성
Matcher matcher = pattern.matcher("010-1234-5678"); // 전화번호와 매칭할 매처 생성
while (matcher.find()) { // 모든 일치하는 문자열 추출
  System.out.println(matcher.group()); // 일치하는 문자열 출력
}
```

## 더 깊게 알아보기

정규 표현식은 패턴이 복잡해질수록 이해하기 어려워질 수 있습니다. 그러나 이를 사용하면 문자열을 다루는 작업을 더 효율적으로 할 수 있습니다. 따라서 개발자들은 정규 표현식을 숙지하는 것이 좋습니다. 또한, 아래의 참고 자료를 참고하여 더 많은 정보를 얻을 수 있습니다.

## 더 알아보기

- [정규식(Regular Expressions)이란?](https://coding-factory.tistory.com/529)
- [정규 표현식을 다루기 위한 8가지 팁](https://tech.liip.ch/blog/regular-expressions-8-useful-and-evidenced-tips/)
- [정규 표현식을 공부하기 위한 10가지 사이트](https://chrisalbon.com/python/regular_expressions/regex_sites/)