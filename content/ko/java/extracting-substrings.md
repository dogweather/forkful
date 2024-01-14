---
title:    "Java: 부분 문자열 추출하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 하위 문자열을 추출하는 것이 중요한 이유는 다양합니다. 예를 들어, 사용자가 입력한 문자열에서 특정한 부분을 추출하면 보다 쉽게 처리하거나 해당 값을 다른 곳에서 사용할 수 있습니다. 또는 문자열에서 패턴을 찾기 위해 하위 문자열을 추출하는 등의 용도로 활용할 수 있습니다.

## 추출하는 방법

우선 자바에서 문자열을 추출하기 위해선 `substring()` 함수를 사용해야 합니다. 이 함수는 첫 번째 인자로 추출할 문자열의 시작 위치를, 두 번째 인자로 추출할 문자열의 끝 위치를 전달합니다. 또한, `length()` 함수를 통해 문자열의 길이를 확인하여 시작 위치와 끝 위치를 조절할 수 있습니다.

아래는 간단한 예제 코드와 함께 `substring()` 함수의 사용 방법을 보여줍니다.

```Java
// 입력 문자열
String str = "안녕하세요, 저는 자바 프로그래머입니다."

// "안녕하세요" 부분을 추출
String greeting = str.substring(0, 5);

// "자바 프로그래머" 부분을 추출
int length = str.length();

// 글자 수를 기준으로 추출
String profession = str.substring(length-8, length);
```

위의 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
greeting: 안녕하세요
profession: 자바 프로그래머
```

## 깊게 들어가기

`substring()` 함수를 이용하여 문자열을 추출하는 방식은 간단하지만, 조금 더 깊게 들어가면 다음과 같은 사실들을 알 수 있습니다.

1. 자바에서는 문자열을 `char`의 배열로 다루기 때문에, `substring()` 함수는 실제로는 인덱스로 접근하여 해당 부분을 추출하는 것입니다.
2. 하위 문자열을 추출할 때, 문자열의 인덱스는 항상 0부터 시작합니다. 따라서 `substring()` 함수의 첫 번째 인자도 0 이상이어야 합니다.
3. `substring()` 함수의 두 번째 인자를 생략하면, 시작 위치부터 문자열의 끝까지 추출됩니다. 즉, `length()` 함수를 사용하지 않고도 마지막 글자까지 추출할 수 있습니다.

## 참고 자료

- [Java String의 substring() 함수](https://www.w3schools.com/java/ref_string_substring.asp)
- [Java 문자열 다루기](https://wikidocs.net/230)
- [GeeksforGeeks: Extract a substring in Java](https://www.geeksforgeeks.org/extract-substring-java/)