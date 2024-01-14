---
title:    "Java: 디버그 출력 출력하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜

디버그 출력을 인쇄하는 이유는 코드의 실행 동안 발생하는 이벤트를 추적하고 문제를 해결하는 데 도움이 됩니다.

## 하는 방법

디버그 출력을 인쇄하는 간단한 예제를 살펴보겠습니다. 먼저 디버그 출력을 인쇄할 Java 코드를 준비해야 합니다. 코드의 맨 위에는 다음과 같이 대문자로 된 상수 DEBUG를 선언합니다.

```Java
final int DEBUG = 1;
```

그다음, 디버그 출력을 인쇄할 수 있는 조건문을 추가합니다. 예를 들어, 코드의 특정 부분에서 변수 i의 값을 출력하려면 다음과 같이 작성할 수 있습니다.

```Java
if(DEBUG == 1){
    System.out.println("i의 값: " + i);
}
```

위의 코드에서는 DEBUG 변수의 값을 1로 설정하면 디버그 출력이 발생합니다. 이렇게 하면 디버깅이 끝나면 DEBUG 변수의 값을 0으로 변경하면 됩니다.

## 딥 다이브

디버그 출력은 코드의 실행 과정을 추적하고 디버깅을 도와줍니다. 디버그 출력을 통해 코드의 어느 부분에서 문제가 발생했는지 확인할 수 있고, 변수의 값이 어떻게 변하는지 추적할 수 있습니다. 또한, 디버그 출력을 통해 코드의 실행 순서를 확인할 수 있으며, 특정 조건에 따라 다른 출력을 할 수도 있습니다.

## 참고

- [Java의 디버그 출력에 대한 자세한 정보](https://docs.oracle.com/javase/tutorial/essential/exceptions/use.html)
- [디버그 출력이 유용한 상황과 사용 방법에 대한 예제](https://www.geeksforgeeks.org/debug-print-macros-predefined-macros-and-function-like-macros/)
- [Java의 System.out.println() 메소드에 대한 공식 문서](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html#println(boolean))