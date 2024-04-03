---
date: 2024-01-20 17:53:14.840723-07:00
description: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uD504\uB85C\uADF8\uB7A8\uC758\
  \ \uC0C1\uD0DC\uB97C \uD45C\uC2DC\uD558\uB294 \uBA54\uC2DC\uC9C0\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC624\uB958\uB97C \uCC3E\uACE0, \uCF54\
  \uB4DC\uAC00 \uC758\uB3C4\uD55C \uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0 \uD655\
  \uC778\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.052175-06:00'
model: gpt-4-1106-preview
summary: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC740 \uD504\uB85C\uADF8\uB7A8\uC758 \uC0C1\
  \uD0DC\uB97C \uD45C\uC2DC\uD558\uB294 \uBA54\uC2DC\uC9C0\uC785\uB2C8\uB2E4."
title: "\uB514\uBC84\uADF8 \uCD9C\uB825\uC744 \uCC0D\uC5B4\uBCF4\uAE30"
weight: 33
---

## How to: (방법)
자바에서 디버그 출력을 할 때 가장 흔히 사용하는 방법은 `System.out.println()`입니다. 굳이 복잡하게 생각하지 않아도 됩니다. 보시다시피 간단합니다:

```java
public class DebugExample {
    public static void main(String[] args) {
        int a = 3;
        int b = 4;
        System.out.println("The value of a is: " + a);
        System.out.println("The value of b is: " + b);
        int sum = a + b;
        System.out.println("The sum of a and b is: " + sum);
    }
}
```

실행 결과:

```
The value of a is: 3
The value of b is: 4
The sum of a and b is: 7
```

기본이지만 매우 유용하죠. 그러나 혼란스럽게 만들 수 있으니 실제 운영될 코드에는 제거하세요.

## Deep Dive (심화 학습)
디버그 출력은 자바 초기 버전부터 존재했습니다. `System.out.println()` 외에도 `System.out.print()` 또는 `System.out.printf()`로 미세 조정을 할 수 있습니다. 로깅 프레임워크(Log4j, SLF4J 등) 같은 고급 대안들도 있습니다. 이들은 출력을 파일이나 콘솔로 리다이렉트하고, 로그 레벨을 설정할 수 있는 기능을 제공합니다. 

`System.out`은 표준 출력 스트림으로, 보통 콘솔에 연결되어 있습니다. 깊이 파보면 자바의 `PrintStream` 클래스를 사용하는데요, 이 클래스는 다양한 출력 메서드를 제공합니다. 다음은 `System.out.println()`과 `System.err.println()`을 사용해 표준 출력 및 표준 오류 스트림을 보여주는 예제입니다:

```java
public class DebugStreamsExample {
    public static void main(String[] args) {
        System.out.println("This goes to standard output");
        System.err.println("This goes to standard error");
    }
}
```

주의할 점은, `System.err`는 에러 메시지를 출력할 때 사용하며, 표준 출력과 별개로 작동한다는 것입니다. 출력 순서가 다를 수 있으니 주의하세요.

## See Also (추가 자료)
- [Oracle Java Docs - System Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/System.html)
- [Apache Log4j 2](https://logging.apache.org/log4j/2.x/)
- [Introduction to SLF4J](http://www.slf4j.org/manual.html)
