---
title:    "Java: 문자열 대문자로 변환하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

자바 프로그래밍을 할 때, 문자열을 대문자로 변환하는 것이 왜 필요한지 궁금해하시나요? 이 글에서는 문자열을 대문자로 변환하는 이유에 대해 알아보겠습니다.

## 방법

문자열을 대문자로 변환하는 것은 매우 간단합니다. 이 작업을 수행하기 위해서는 ```toUpperCase()``` 메소드를 사용하면 됩니다. 아래의 예시 코드를 살펴보세요.

```Java
String str = "hello world";
str.toUpperCase();
```

위의 코드를 실행하면 "HELLO WORLD"라는 결과가 출력됩니다. 즉, ```toUpperCase()``` 메소드는 문자열을 전부 대문자로 변환합니다.

## 깊게 파헤치기

문자열을 대문자로 변환하는 방법은 ```toUpperCase()``` 메소드를 사용하는 것만으로 충분합니다. 하지만 이 메소드는 문자열에서 모든 문자를 대문자로 변환하므로, 어떤 문자열에서는 원하는 결과를 얻지 못할 수도 있습니다.

예를 들어, 영어에서는 "I"라는 단어를 "i"와 "I"로 구분하지 않지만, 다른 언어에서는 "i"와 "I"를 따로 구분합니다. 따라서 ```toUpperCase()``` 메소드를 사용하면 모든 "i"가 대문자로 변환되어 원하지 않는 결과를 가져올 수 있습니다.

이럴 때는 ```toUpperCase()``` 메소드를 사용하는 대신, ```replace()``` 메소드를 사용하여 해당 문자만 대문자로 변환하는 것이 좋습니다. 아래의 예시 코드를 살펴보세요.

```Java
String str = "내 이름은 Java입니다.";
str.replace("v", "V");
```

위의 코드를 실행하면 "내 이름은 JaVa입니다."라는 결과가 출력됩니다. 즉, ```replace()``` 메소드를 사용하여 문자열 중 특정 문자만 대문자로 변환할 수 있습니다.

## 관련 링크

- [Java String 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [replace() 메소드 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
- [toUpperCase() 메소드 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)