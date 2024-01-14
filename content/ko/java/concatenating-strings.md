---
title:                "Java: 문자열 연결하기"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

작성된 언어를 사용하는 프로그래머라면 당신은 문자열을 연결해보았을 것입니다. 하지만 왜 문자열 연결을 하는 것인지 궁금하셨나요? 이 블로그 포스트에서는 이에 대해 알아보겠습니다.

## 어떻게

먼저, 문자열 연결의 기본적인 사용법을 알아보겠습니다. 코드 블록에서 ```Java``` 를 사용하여 예제와 출력을 보여줄 것입니다.

```Java
String str1 = "Hello ";
String str2 = "World!";
String result = str1 + str2;
System.out.println(result);
```
출력:
```
Hello World!
```

또한, 문자열 연결의 강력한 기능 중 하나는 변수 뿐만 아니라 바로 값을 넣어 연결할 수 있다는 점입니다. 예를 들어,

```Java
String name = "John";
int age = 25;
String result = "My name is " + name + " and I am " + age + " years old.";
System.out.println(result);
```

출력:
```
My name is John and I am 25 years old.
```

## 더 알아보기

문자열 연결은 String 클래스의 메소드인 concat()과 비슷합니다. 하지만 문자열 연결을 사용하면 더 간편하고 쉽게 문자열을 합칠 수 있습니다. 또한 문자열 연결은 메모리를 더 효율적으로 관리할 수 있습니다.

## 더 알아보기

이 블로그 포스트는 문자열 연결의 기본적인 사용법과 강력한 기능에 대해 소개해드렸습니다. 하지만 문자열 연결의 더 많은 기능들을 이용해보시길 추천합니다. 더 많은 정보는 아래의 링크를 확인해주세요.

## 같이 보기

- [Java String 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java 문자열 연결의 효율성에 대한 논문](https://www.cs.nyu.edu/courses/spring11/CSCI-GA.2110-002/lecture7b.pdf)