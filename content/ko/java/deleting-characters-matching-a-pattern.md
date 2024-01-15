---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Java: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열 매칭 패턴과 일치하는 문자를 삭제하는 작업을 수행하는 이유는 특정한 문자열에서 원하지 않는 문자를 제거하기 위해서입니다. 예를 들어, 사용자가 입력한 정보에서 특정한 문자 또는 공백을 제거하여 데이터를 정제하기 위해서 이 작업이 수행될 수 있습니다.


## 어떻게

가장 먼저, 삭제할 문자열을 포함한 문자열을 지정합니다. 그리고 해당 문자열에서 삭제할 패턴을 지정하고 해당 패턴을 매칭하는 문자를 삭제하도록 코드를 작성합니다. 아래는 문자열에서 숫자를 제거하는 예제 코드와 출력 결과입니다.
```Java
String str = "He11o W0rld!";
String pattern = "\\d+";
// 코드를 작성하여 문자열에서 숫자를 제거하는 작업을 수행합니다.
System.out.println(str); // 출력 결과: He11o W0rld!
```

## 깊이있게 살펴보기

정규 표현식을 사용하여 문자열 매칭 패턴을 지정할 수 있습니다. 위의 예제에서 사용한 "\\d+"는 숫자를 나타내는 정규 표현식입니다. "d"는 숫자를 나타내는 메타 문자이고 "+"는 해당 문자가 한 번 이상 나타날 수 있음을 나타냅니다. 따라서 오직 숫자만을 포함하는 패턴에 매칭됩니다. 정규 표현식을 적절히 사용하면 원하는 문자를 더욱 쉽게 삭제할 수 있습니다. 

## 더 알아보기

- [Java 정규 표현식 사용법](https://www.javatpoint.com/java-regex)
- [문자열 다루기](https://www.w3schools.com/java/java_strings.asp)

## 참고

Java에서 문자열을 다루는 방법은 다양합니다. 이 문서에서는 정규 표현식을 사용하여 문자열에서 패턴을 매칭하는 방식을 다루었지만 다른 방식 또한 존재합니다. 적절한 방식을 선택하여 사용하시면 됩니다.