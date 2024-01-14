---
title:    "Java: 문자열의 길이 찾기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜

자바 프로그램을 개발하는 동안 문자열의 길이를 계산하는 것이 중요한 이유는 여러 가지가 있습니다. 첫째, 문자열의 길이를 알면 원하는 출력 형식을 만들 수 있습니다. 둘째, 입력으로 받은 문자열의 길이를 알면 프로그램의 실행 흐름을 제어할 수 있습니다.

# 어떻게

필자는 간단한 예제를 통해 어떻게 문자열의 길이를 계산하는지 알려드리겠습니다. 아래의 코드 블록을 보시면서 따라해보세요.

```Java
String word = "안녕하세요!";
int length = word.length(); // 길이를 계산하여 변수에 저장
System.out.println("문자열의 길이는 " + length + "입니다."); // 출력: 문자열의 길이는 7입니다.
```

이 코드에서 우리는 `length()` 메소드를 사용하여 `word` 변수에 저장된 문자열의 길이를 계산합니다. 그리고 `println()` 메소드를 사용하여 문자열의 길이를 출력합니다.

# 깊이 있는 설명

자바에서 문자열의 길이를 계산하는 메소드는 `length()` 메소드 뿐만 아니라 `getBytes()` 메소드도 있습니다. `length()` 메소드는 문자열의 길이를 측정할 때 유니코드 문자의 수를 기준으로 하지만 `getBytes()` 메소드는 문자열을 인코딩할 때 필요한 바이트 수를 반환합니다. 이러한 차이점을 알고 있으면 프로그램을 개발할 때 효율적으로 문자열을 다룰 수 있습니다.

# 참고 자료

- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Understanding the Java String API](https://www.baeldung.com/java-string-api)
- [Java String 클래스를 이용한 문자열 다루기](https://codechacha.com/ko/java-string-manipulation/)
- [Java 문자열 변수 선언하기](http://www.tcpschool.com/java/java_operator_string)