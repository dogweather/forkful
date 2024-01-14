---
title:                "Java: 부분 문자 추출하기"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

서브 문자열을 추출하는 것에 대해 왜 관심을 가지게 될까요?
서브 문자열 추출은 자바 프로그래밍에서 매우 유용한 기술입니다. 문자열 내에서 특정 부분만을 추출하여 원하는 작업을 수행할 수 있기 때문입니다.

## 왜

서브 문자열의 추출은 다양한 상황에서 유용합니다. 예를 들어, 어떤 문자열에서 특정 단어만을 추출하거나 문자열 내에서 원하는 부분을 검색하는 등의 작업에 활용할 수 있습니다. 이를테면, 사용자의 입력 데이터를 분석하거나 특정 문자열을 자동으로 변경하기 위해 서브 문자열 추출을 사용할 수 있습니다.

## How To

서브 문자열 추출을 위해선 자바의 `substring()` 메서드를 이용합니다. 이 메서드는 문자열에서 지정한 범위를 추출하여 새로운 문자열로 반환합니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

```java
String str = "안녕하세요, 친구들?";
String subStr = str.substring(0, 3); // 첫 3글자 추출
System.out.println(subStr); // 출력 결과: 안녕
```

또한, `indexOf()` 메서드와 `lastIndexOf()` 메서드를 함께 사용하여 문자열 내에서 특정 부분을 찾아 추출할 수도 있습니다. 이를테면, 다음과 같이 사용할 수 있습니다.

```java
String str = "나의 행복한 가족들";
int startIndex = str.indexOf("행복한"); // "행복한"이 시작하는 인덱스
int endIndex = str.lastIndexOf("들"); // "들"이 마지막으로 나오는 인덱스
String subStr = str.substring(startIndex, endIndex); // "행복한 가족" 추출
System.out.println(subStr); // 출력 결과: 행복한 가족
```

## Deep Dive

`substring()` 메서드를 활용할 때 주의할 점이 있습니다. 첫 번째 인덱스는 포함되지만, 두 번째 인덱스는 포함되지 않는다는 것입니다. 또한, 두 번째 인덱스를 생략할 경우 끝까지 문자열을 추출합니다. 예를 들어, 다음과 같은 코드를 실행하면 예상치 못한 결과를 얻게 될 수 있습니다.

```java
String str = "Hello world!";
String subStr = str.substring(6); // 6번 인덱스부터 끝까지 추출
System.out.println(subStr); // 출력 결과: world!
```

이러한 점을 고려하여 적절한 인덱스를 지정하여 `substring()` 메서드를 활용해야 합니다.

## See Also

- [Oracle Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html): 자바의 `String` 클래스에 대한 공식 문서
- [Java String Tutorial](https://www.javatpoint.com/java-string): 자바 문자열 관련 튜토리얼 리소스
- [Substring vs Subsequence in Java](https://www.geeksforgeeks.org/substring-vs-subsequence-java/): 자바에서 서브 문자열과 서브 시퀀스의 차이점을 설명하는 기사.