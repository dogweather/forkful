---
title:                "Java: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜 무작위 숫자를 생성하는가

무작위 숫자를 생성하는 것은 프로그래밍에서 매우 유용합니다. 예를 들어, 데이터를 모델링하고 시뮬레이션을 할 때, 무작위 숫자를 사용하여 실제 세계를 모방할 수 있습니다. 또한 무작위 숫자는 게임 개발에서 무작위 이벤트를 구현하는 데 사용될 수 있습니다. 이러한 이유로 무작위 숫자 생성은 매우 중요합니다.

## 어떻게 하는가

Java에서는 무작위 숫자를 생성하는 데에 대한 내장 클래스가 있습니다. java.util 패키지에는 Random 클래스가 있으며, 이를 사용하면 무작위 숫자를 쉽게 생성할 수 있습니다.

```Java
import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {
        // Random 클래스의 객체를 생성합니다.
        Random rand = new Random();

        // 0 이상 10 미만의 무작위 정수를 생성합니다.
        int num1 = rand.nextInt(10);

        // 1 이상 100 미만의 무작위 실수를 생성합니다.
        double num2 = rand.nextDouble() * 100;
        
        System.out.println("무작위 정수: " + num1);
        System.out.println("무작위 실수: " + num2);
    }
}
```

위의 코드를 실행하면 매번 다른 무작위 숫자가 생성됩니다. 이를 통해 무작위 숫자 생성이 얼마나 간단한지 알 수 있습니다.

## 심층 분석

Random 클래스의 nextInt(), nextDouble() 메소드는 내부적으로 seed 값을 사용하여 무작위 숫자를 생성합니다. seed 값은 기본적으로 시스템 시간을 사용하며, 이를 변경하여 다른 무작위 숫자를 생성할 수도 있습니다. 예를 들어, 동일한 seed 값을 사용하면 항상 같은 무작위 숫자가 생성됩니다. 따라서, 시스템 시간이 정확하게 일치하는 경우에는 무작위 숫자가 겹칠 수 있습니다.

또한 Random 클래스는 무작위 수를 생성하기 위한 여러 가지 메소드를 제공합니다. 메소드에 매개변수를 전달하여 원하는 범위의 무작위 숫자를 생성할 수 있습니다. 이 외에도 다양한 기능을 제공하므로, 더 많은 정보는 [Java documentation for Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)을 참조하세요.

## 참고 자료

- [Java documentation for Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java - 난수 생성하기(Random)](https://jeong-pro.tistory.com/123)