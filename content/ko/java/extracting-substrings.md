---
title:                "부분 문자열 추출"
html_title:           "Java: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜? 

서브스트링을 추출하는 이유는 문자열 처리와 관련하여 자주 사용하는 기능 중 하나이기 때문입니다. 이를 통해 사용자는 문자열에서 원하는 부분만을 선별적으로 가져와 다양한 용도로 활용할 수 있습니다.

## 어떻게 할까요?

```Java
// 문자열에서 일부분 추출하기
String str = "Hello, world!";
String sub = str.substring(2, 6); // 결과: "llo," 
System.out.println(sub); // "llo,"
```

```Java
// 변수를 사용하여 추출 범위 지정하기
String str = "Java programming";
int start = 5; // 추출을 시작할 인덱스
int end = 12; // 추출을 끝낼 인덱스 + 1
String sub = str.substring(start, end); // 결과: "program"
System.out.println(sub); // "program"
```

자바에서는 `substring()` 메소드를 사용하여 원하는 범위의 문자열을 추출할 수 있습니다. 이때, 첫 번째 매개변수는 추출을 시작할 인덱스를, 두 번째 매개변수는 추출을 끝낼 인덱스의 다음 값을 지정합니다. 위의 예시에서는 `str` 문자열에서 5번째 인덱스부터 12번째 인덱스의 이전 값까지인 "program"을 추출한 것입니다.

```Java
// 문자열의 끝까지 추출하기
String str = "Korean language";
String sub = str.substring(7); // 결과: "language"
System.out.println(sub); // "language"
```

추출 범위를 지정하지 않고 첫 번째 매개변수만 사용하여 추출하면 해당 인덱스부터 문자열의 끝까지를 추출할 수 있습니다. 위의 예시에서는 `str` 문자열에서 7번째 인덱스부터 끝까지인 "language"를 추출한 것입니다.

## 더 자세히 알아보기

자바에서는 `substring()` 메소드 외에도 `subSequence()` 메소드를 사용하여 문자열 부분을 추출할 수 있습니다. 또한, 추출한 문자열을 다른 타입으로 변환하는 방법도 존재합니다. `substring()` 메소드의 반환값이 `String` 타입이기 때문에, `parseInt()` 메소드를 사용하여 추출한 문자열을 `int` 타입으로 변환하는 등 다양하게 활용할 수 있습니다.

## 이외에도 참고할 링크들

- [Oracle Java Documentation: String class](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html)
- [Tutorials Point: Java - String substring() Method](https://www.tutorialspoint.com/java/java_string_substring)