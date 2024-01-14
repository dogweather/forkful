---
title:                "Java: 문자열 길이 찾기"
simple_title:         "문자열 길이 찾기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜

 문자열의 길이를 찾는 것에 대해 먼저 이야기해보겠습니다. 이것은 프로그래밍에서 매우 기본적이며 중요한 개념입니다. 우리는 종종 문자열의 길이를 알아야할 때가 있고, 그것을 쉽게 찾을 수 있는 방법을 알아둬야합니다. 자바에서 문자열의 길이를 찾는 방법에 대해 알아보겠습니다.

## 방법

```Java
String str = "안녕하세요";
int length = str.length();
System.out.println("문자열의 길이는 " + length + "입니다.");
```

위의 예시처럼 문자열의 길이를 찾는 방법은 매우 간단합니다. 우선 문자열을 변수에 할당해준 다음, `.length()` 메소드를 사용해서 그 길이를 찾으면 됩니다. 이번에는 "안녕하세요"라는 문자열의 길이를 측정해보았는데, 총 5개의 문자가 포함되어 있으므로 결과 값은 5가 됩니다.

```Java
String str = "Hello, World!";
int length = str.length();
System.out.println("문자열의 길이는 " + length + "입니다.");
```

또 다른 예시를 살펴보면, "Hello, World!"라는 문자열의 경우에는 총 13개의 문자가 포함되어 있으므로 결과 값은 13이 나옵니다. 이렇듯 어떤 문자열이던간에 `.length()` 메소드를 사용하면 간단하게 그 길이를 찾을 수 있습니다.

## 깊이 파헤치기

자바에서 문자열의 길이를 찾는 방법을 좀 더 자세히 알아보겠습니다. `.length()` 메소드는 문자열의 길이를 반환해주는 메소드입니다. 그리고 이 메소드는 `String` 클래스의 `length()` 메소드를 호출하는 방식으로 구현되어 있습니다. `length()` 메소드는 `String` 클래스 내부에 존재하는 private한 멤버 변수인 `count`의 값을 반환해줍니다. 이 멤버 변수는 실제로 문자열의 길이를 저장하는 역할을 하고 있습니다. 따라서 이렇게 `.length()` 메소드를 호출하면 `count`의 값이 반환되어 그 결과로 문자열의 길이를 알 수 있게 됩니다.

# 참조

- [How to get the length of a string in Java](https://www.baeldung.com/java-string-length)
- [String.length() method in Java](https://www.geeksforgeeks.org/string-length-method-in-java/)