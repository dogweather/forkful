---
title:    "Java: 난수 생성하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜
랜덤 넘버를 생성하는 일을 하는 이유는 다양합니다. 예를 들어, 게임에서 캐릭터나 아이템 등을 랜덤으로 생성하는 경우가 있습니다. 또한 랜덤 넘버를 사용해 데이터의 샘플링이나 시뮬레이션을 할 수도 있습니다.

## 방법
랜덤 넘버를 생성하는 방법은 다양하지만, Java에서는 `java.util.Random` 클래스를 사용하면 간단하게 만들 수 있습니다. 먼저 `Random` 객체를 생성하고 `nextInt()` 메소드를 호출하여 랜덤한 정수를 얻을 수 있습니다.

```Java
import java.util.Random;

Random random = new Random();
int randomInt = random.nextInt();
System.out.println(randomInt);
```

또한 `Random` 클래스의 생성자에 시드 값을 전달하여 랜덤 넘버의 시퀀스를 설정할 수도 있습니다.

```Java
Random random = new Random(1234); // 시드 값은 임의로 지정 가능
int randomInt = random.nextInt();
```

## 깊이 들어가기
랜덤 넘버를 생성하는 과정에서 많은 알고리즘이 사용됩니다. 예를 들어, `Random` 클래스에서는 가장 널리 사용되는 선형 합동법 (linear congruential method)를 사용합니다. 이 방법은 현재 시퀀스의 이전 결과를 사용하여 다음 랜덤 넘버를 생성하는 방식입니다. 따라서 시드 값을 설정하면 항상 같은 시퀀스의 넘버를 생성할 수 있습니다.

Random 클래스는 기본적으로 현재 시간을 시드 값으로 사용하므로 매번 다른 랜덤 넘버를 생성할 수 있습니다. 하지만 같은 시간에 실행될 경우 같은 넘버가 생성될 수 있으므로, 시드 값을 직접 설정하는 것이 더 안전합니다.

## 참고 자료
- [How to Generate Random Numbers in Java](https://www.baeldung.com/java-generate-random-long-float-integer-double)
- [Java Random Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)