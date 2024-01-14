---
title:                "Java: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 번째 문자를 대문자로 변환하는 것은 코드의 일부로 자주 사용되고 있습니다. 이를테면, 이름과 성을 조합하여 전체 이름을 나타내는 경우에 사용합니다.

## 방법

우선, 문자열을 입력받은 다음, 첫 번째 문자를 대문자로 변환하고 나머지 문자열과 합쳐야 합니다. 이를 위해 Java에서 제공하는 `substring()` 메소드를 사용할 수 있습니다. 아래는 코드 예시와 출력 결과입니다.

```Java
String name = "john";
String capitalized = name.substring(0, 1).toUpperCase() + name.substring(1);
System.out.println(capitalized); 
// Prints "John"
```

## 깊게 파헤치기

위에서 소개한 방법은 간단하지만, 실제로 문자열의 첫 번째 문자만 대문자로 변환하는 것은 더 복잡합니다. 이는 특정 언어의 규칙에 따라 다르기 때문입니다. 예를 들어, 한국어의 경우에는 이름의 마지막 글자를 대문자로 변환해주는 경우가 많습니다. 따라서, 사용하고자 하는 언어의 이름 변환 규칙을 잘 알고 코드를 작성해야 합니다.

## 또 다른 정보

- [Java String class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java substring() method explanation](https://www.javatpoint.com/java-string-substring)
- [Java String manipulation examples](https://www.baeldung.com/java-strings)