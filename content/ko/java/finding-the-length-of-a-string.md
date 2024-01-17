---
title:                "문자열의 길이 찾기"
html_title:           "Java: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇인가요? & 왜 이것을 해야 하나요?

문자열의 길이를 찾는 것은 입력된 문자열의 길이를 알아내는 것입니다. 이는 프로그래머가 자주 사용하는 작업 중 하나이며, 해당 문자열이 얼마나 긴지 또는 몇 개의 단어로 이루어져 있는지를 파악하기 위해서입니다.

## 어떻게 하나요?

```Java
public class StringLength {
  public static void main(String[] args) {
    // 문자열의 길이를 알고 싶은 문자열 입력
    String myString = "Hello World";
    // .length() 메소드를 사용하여 문자열 길이 출력
    System.out.println("문자열의 길이는: " + myString.length());
  }
}
```

```
출력: 문자열의 길이는: 11
```

## 더 깊게 알아보기

1. [Java API 문서](https://docs.oracle.com/javase/8/docs/api/)에서 `String.length()` 메소드를 찾아보세요.
2. `length()` 메소드는 String 클래스에 내장된 메소드이기 때문에 따로 import할 필요 없이 사용할 수 있습니다.
3. `length()` 메소드는 문자열의 길이를 알기 위해 실제로 매번 문자열을 순회하지 않고, 내부적으로 저장된 문자열의 길이를 반환합니다.

## 참고 자료

- [GeeksforGeeks: Java String length() Method](https://www.geeksforgeeks.org/java-string-length-method-example/): 문자열 길이를 구하는 예제와 설명이 있습니다.
- [Baeldung: String Length in Java](https://www.baeldung.com/java-string-length): 문자열 길이를 확인하는 다른 방법들을 소개하는 글입니다.