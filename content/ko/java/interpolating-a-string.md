---
title:                "문자열 보간"
html_title:           "Java: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열 보간은 문자열 내에 다른 변수나 표현식을 삽입하는 것을 말합니다. 이를 통해 우리는 더 동적인 문자열을 만들 수 있습니다. 예를 들어, 사용자의 이름이 포함된 문자열을 출력하고 싶을 때, 우리는 문자열 보간을 사용하여 변수에 저장된 사용자의 이름을 문자열에 삽입합니다.

프로그래머들은 문자열 보간을 사용하는 이유는 자동으로 변수나 표현식을 문자열에 삽입할 수 있기 때문입니다. 이를 통해 코드를 더 깔끔하고 읽기 쉽게 만들어줍니다.

## 방법:

```java
String name = "John";
System.out.println("Hi, my name is ${name}!"); // 출력: Hi, my name is John!
```

위의 예시에서, 우리는 문자열 보간을 사용하여 변수를 쉽게 삽입할 수 있습니다. 이를 통해 우리는 더 동적인 문자열을 만들 수 있으며, 코드를 더 읽기 쉽게 만들어줍니다.

## 깊이 들어가기:

### 역사적 배경:

## 보다 많은 정보

우리는 문자열 보간을 사용하여 변수나 표현식을 쉽게 문자열에 삽입할 수 있습니다. 하지만, 다른 방법들도 있습니다. 다른 방법으로는 문자열 연결, 서식 지정 문자열과 String.format() 메서드를 사용하는 것이 있습니다.

또한, 문자열 보간은 자바 15 버전부터 지원하며, 이전 버전에서는 사용할 수 없습니다. 따라서 이전 버전의 자바를 사용하고 있다면, 다른 방법을 사용해야 합니다.

## 관련 자료:

- https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#formatted-strings
- https://www.baeldung.com/java-string-interpolation