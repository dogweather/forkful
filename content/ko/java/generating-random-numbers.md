---
title:                "Java: 랜덤 숫자 생성하기"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜?

난수 생성에 참여하는 이유는 무엇일까요? 난수 생성은 프로그래밍에서 매우 유용한 기능입니다. 난수 생성을 통해 우리는 데이터를 무작위로 다룰 수 있고 더 다양한 알고리즘을 만들 수 있습니다. 이를 통해 우리의 코드를 더 효율적이고 강력하게 만들 수 있습니다.

## 하우 투?

```Java
import java.util.Random;

public class RandomNumberGenerator {

    public static void main(String[] args) {
        Random random = new Random();
        // 정수형 난수 생성
        int randomInt = random.nextInt();
        System.out.println("Integer Random Number: " + randomInt);
        // 범위를 지정한 정수형 난수 생성
        int randomIntRange = random.nextInt(10);
        System.out.println("Integer Random Number with Range: " + randomIntRange);
        // double 타입의 난수 생성
        double randomDouble = random.nextDouble();
        System.out.println("Double Random Number: " + randomDouble);
        // 임의의 boolean 값 생성
        boolean randomBoolean = random.nextBoolean();
        System.out.println("Random Boolean Value: " + randomBoolean);
    }
}

```

위 코드는 Java에서 난수를 생성하는 간단한 예시입니다. 우리는 먼저 `java.util.Random` 라이브러리를 import 하고, `Random` 객체를 생성합니다. `Random` 객체를 사용하면 우리는 다양한 난수 생성 기능을 사용할 수 있습니다. `nextInt()` 함수를 사용하면 정수형 난수를 생성할 수 있고, `nextDouble()` 함수를 사용하면 double 타입의 난수를 생성할 수 있습니다. 또한 `nextBoolean()` 함수를 사용하여 임의의 boolean 값을 생성할 수도 있습니다. 마지막으로 `nextInt()` 함수에 정수 값을 넘겨 범위를 지정하여 원하는 범위의 정수형 난수를 생성할 수도 있습니다.

위 코드를 실행시키면 다음과 같은 결과가 나올 수 있습니다.

```
Integer Random Number: -1347613908
Integer Random Number with Range: 4
Double Random Number: 0.7790521801740829
Random Boolean Value: true
```

위 예시는 Java에서 난수를 생성하는 간단한 방법을 보여줍니다. 하지만 난수 생성은 이보다 더 복잡할 수 있습니다. 다음 섹션에서는 더 깊이 들어가서 난수 생성에 대해 더 자세히 알아보겠습니다.

## 딥 다이브

우리는 이전 섹션에서 `java.util.Random` 라이브러리를 이용해 난수를 생성하는 간단한 예시를 살펴보았습니다. 하지만 이 방법은 매우 기본적인 방법이며, 더 복잡한 난수 생성 기법을 사용할 수도 있습니다. 다음은 더 깊이 들어가서 우리가 난수 생성을 할 때 알아야 할 몇 가지 중요한 개념들입니다.

### 의사 난수 생성기

난수 생성은 사실 의사 난수를 생성하는 것입니다. 이 의사 난수는 사실은 특정한 알고리즘에 의해 생성되는 수열입니다. 따라서 우리는 이 의사 난수 생성기의 알고리즘을 바꾸어 다양한 난수를 생성할 수 있습니다.

### 시드(seed)

의사 난수 생성기는 랜덤한 시작 숫자를 이용하는데, 이 숫자를 시드(seed)라고 합니다. 우리는 이 시드를 지정하여 매번 같은 난수를 생성할 수 있습니다. 이는