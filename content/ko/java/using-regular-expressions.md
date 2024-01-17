---
title:                "정규 표현식 사용하기"
html_title:           "Java: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 무엇이고 왜요?

정규 표현식 이용은 프로그래머들이 문자열에서 원하는 패턴을 찾아내기 위해 사용하는 것입니다. 이를 사용하면 보다 쉽고 빠르게 패턴을 찾아내고 조작할 수 있습니다. 

# 어떻게 해요?

```Java
// 패턴과 일치하는 문자열 찾기
String regex = "cat";
String testString = "I have a cat named Fluffy.";
Pattern pattern = Pattern.compile(regex);
Matcher matcher = pattern.matcher(testString);

// 패턴을 이용한 문자열 치환
String replacedString = matcher.replaceAll("dog");
```

예제 출력:

```
I have a dog named Fluffy.
```

# 깊게 살펴보기

- 역사적 배경: 정규 표현식은 프로그래밍 언어와는 별개로 원래 수학에서 유래한 개념입니다. 그러나 현대 프로그래밍에서는 많은 언어에서 지원하고 있습니다.
- 대안: 정규 표현식보다 더 간단한 문자열 처리를 위해서는 정규 표현식 대신 문자열 메서드를 사용할 수 있습니다.
- 구현 세부 사항: 정규 표현식은 패턴과 문자열 매칭에 대해 매우 유연한 문법을 제공하므로 학습이 필요한 경우가 많습니다.

# 관련 자료

- [Java 정규 표현식 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [정규식 안내서](https://regexone.com/)
- [라이브러리 : java.util.regex](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)