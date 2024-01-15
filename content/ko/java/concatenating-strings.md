---
title:                "문자열 연결"
html_title:           "Java: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것은 프로그래밍에서 중요한 작업 중 하나입니다. 문자열을 연결하는 이유는 다양합니다. 예를 들어, 두 개의 문자열을 하나의 문장으로 만들기 위해 사용하거나, 사용자 혹은 시스템 입력을 문자열로 변환할 때 사용합니다. 이러한 작업들을 수행하기 위해 문자열을 연결하면 효율적으로 원하는 결과를 얻을 수 있습니다.

## 하우 투

두 개의 문자열을 연결하는 기본적인 방법은 `+` 연산자를 사용하는 것입니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다:

```Java
String str1 = "Hello";
String str2 = "World";
String result = str1 + " " + str2;
System.out.println(result);
```

위 코드의 출력결과는 `Hello World`가 됩니다.

또 다른 방법은 `concat()` 메소드를 사용하는 것입니다. 이 메소드는 두 개의 문자열을 연결하는 데 사용됩니다. 다음과 같이 코드를 작성할 수 있습니다:

```Java
String str1 = "Hello";
String str2 = "World";
String result = str1.concat(" ").concat(str2);
System.out.println(result);
```

위 코드의 출력결과 역시 `Hello World`가 됩니다.

## 딥 다이브

문자열을 연결하는 방법은 `StringBuilder` 클래스를 이용하는 것입니다. 이 클래스는 문자열을 조작할 때 메모리와 시간을 절약하는 데 사용됩니다. 또한 문자열을 불변하는 `String` 클래스와 달리, `StringBuilder` 클래스는 가변적인 문자열을 다룰 수 있습니다. 이 클래스의 `append()` 메소드를 사용하여 문자열을 연결할 수 있습니다. 다음과 같은 코드를 작성할 수 있습니다:

```Java
StringBuilder sb = new StringBuilder();
sb.append("Hello");
sb.append(" ");
sb.append("World");
System.out.println(sb.toString());
```

위 코드의 출력결과는 역시 `Hello World`가 됩니다.

## 참고

- [Java Tutorial: Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java StringBuilder class](https://www.geeksforgeeks.org/java-stringbuilder-class/)
- [Java String concat() method](https://www.geeksforgeeks.org/java-string-concat-method/)