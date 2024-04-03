---
date: 2024-01-26 00:54:04.528182-07:00
description: "\uC5B4\uB5BB\uAC8C: \uC790\uBC14\uB294 \uC608\uC678\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uC624\uB958\uB97C \uCC98\uB9AC\uD569\uB2C8\uB2E4. \uC704\uD5D8\uD55C\
  \ \uCF54\uB4DC \uC8FC\uBCC0\uC5D0 `try` \uBE14\uB85D\uC744 \uC0AC\uC6A9\uD558\uACE0\
  \ `catch`\uB85C \uC608\uC678\uB97C \uC7A1\uC2B5\uB2C8\uB2E4. \uAC04\uB2E8\uD55C\
  \ \uC608\uC81C\uB97C \uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.059662-06:00'
model: gpt-4-1106-preview
summary: "\uC790\uBC14\uB294 \uC608\uC678\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC624\uB958\
  \uB97C \uCC98\uB9AC\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 어떻게:
자바는 예외를 사용하여 오류를 처리합니다. 위험한 코드 주변에 `try` 블록을 사용하고 `catch`로 예외를 잡습니다. 간단한 예제를 보겠습니다:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("결과는: " + result);
        } catch (ArithmeticException e) {
            System.out.println("이런, 0으로 나눌 수 없습니다!");
        }
    }

    private static int divide(int numerator, int denominator) {
        return numerator / denominator;
    }
}
```

출력:
```
이런, 0으로 나눌 수 없습니다!
```

## 심화 학습
자바에서의 오류 처리는 발전해 왔습니다. 초기에는 예외가 없었고, 프로그래머들은 오류 코드를 확인했습니다. 그 후 자바는 try-catch 블록을 도입하여 더 우아한 오류 처리를 가능하게 했습니다.

전통적인 `try-catch` 대안으로는 자동으로 자원을 닫고 코드를 더 깔끔하게 만들어주는 `try-with-resources`가 있으며, 자바 7에서 소개되었습니다.

구현 세부 사항이 중요합니다. 예를 들어, `Exception` 또는 `Throwable`을 잡는 것은 일반적으로 나쁜 관행입니다. 너무 광범위하여 인지하지 못하는 버그를 숨길 수 있습니다. 구체적인 예외에 집중하세요.

## 참고자료
- 예외에 대한 공식 오라클 자바 튜토리얼: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- 자바의 `try-with-resources` 문서화: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- 예외에 대한 모범 사례에 관한 책, 조슈아 블로크의 Effective Java.
