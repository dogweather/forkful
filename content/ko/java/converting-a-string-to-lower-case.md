---
title:    "Java: 문자열을 소문자로 변환하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
자바 프로그래밍에서 문자열을 소문자로 변환하는 것은 문자열 처리에 매우 유용합니다. 이를 통해 문자열을 보다 쉽게 비교하고 처리할 수 있습니다.

## 방법
```Java
 public static void main(String[] args) {
   String name = "SEOUL";
   System.out.println("원본 문자열: " + name);

   String lowerCaseName = name.toLowerCase();
   System.out.println("소문자로 변환한 문자열: " + lowerCaseName);
 }
```

```
원본 문자열: SEOUL
소문자로 변환한 문자열: seoul
```

위 코드는 `toLowerCase()` 메소드를 사용하여 문자열을 소문자로 변환하는 간단한 예시입니다. 문자열을 소문자로 변환하려면 `toLowerCase()` 메소드 뿐만 아니라 `toLowerCase(Locale.getDefault())` 메소드를 사용하여 기본 로케일을 적용할 수도 있습니다.

## 깊이 파고들기
문자열을 소문자로 변환하는 과정은 알파벳 문자에 해당하는 유니코드 숫자를 변경하는 것으로 이루어집니다. 대문자 `A`는 유니코드 숫자 65를 가지고 있고, 소문자 `a`는 유니코드 숫자 97을 가지고 있습니다. 따라서 `toLowerCase()` 메소드는 현재 문자의 유니코드 값을 변환하여 소문자로 출력합니다. 이러한 변환을 위해 `Character.toLowerCase()` 메소드를 사용합니다.

## 같이 보기
- [Java String 클래스 API 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java 변수와 자료형에 대한 이해](https://noanswercode.tistory.com/1)
- [Java 알고리즘을 위한 자료구조](https://howtodoinjava.com/algorithms/basics-of-algorithms-and-data-structures/)