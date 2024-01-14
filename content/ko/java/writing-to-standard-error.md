---
title:    "Java: 표준 오류에 쓰는 방법"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜

일반적으로, Java 프로그래밍에서는 우리가 기본적으로 출력을 표준 출력으로 하게됩니다. 하지만 때로는, 표준 오류를 통해 출력하는 것이 더 효과적일 수도 있습니다. 이 글에서는 왜 우리가 표준 오류에 쓰는 것이 유용할 수 있는지 알아보고, 그 방법을 살펴볼 것입니다.

## 어떻게

표준 오류에 쓰는 방법은 매우 간단합니다. 아래의 코드를 보세요:

```Java
public class Main {

  public static void main(String[] args) {
    System.err.println("이것은 표준 오류로 출력됩니다.");
  }
  
}
```

위 코드를 실행하면, "이것은 표준 오류로 출력됩니다."라는 메시지가 표준 오류로 출력됩니다. 표준 오류는 표준 출력과는 별도의 채널이기 때문에, 출력 결과가 따로 분리되어서 보여지게 됩니다.

## 깊이 파고들기

표준 오류를 사용하면, 오류 메시지와 관련된 정보를 더욱 쉽게 파악할 수 있습니다. 예를 들어, 아래 코드를 보세요:

```Java
public class Main{

  public static void main(String[] args) {
    try {
      int result = divide(10, 0);
      System.out.println("결과: " + result);
    } catch (ArithmeticException e) {
      System.err.println("나누기 오류 발생: " + e.getMessage());
    }
  }
  
  public static int divide(int num1, int num2) {
    return num1 / num2;
  }
  
}
```

위 코드를 실행하면, "나누기 오류 발생: / by zero"라는 메시지가 표준 오류로 출력됩니다. 이렇게 표준 오류를 사용하면, 프로그램에서 발생한 오류에 대한 정보를 바로 확인할 수 있어 디버깅에 더욱 유용합니다.

## 관련 자료

- [Java 표준 출력과 오류에 쓰기](https://www.baeldung.com/java-write-to-file)
- [Java 입출력 표준 입력, 출력, 오류](https://codechacha.com/ko/java-io-standards/)