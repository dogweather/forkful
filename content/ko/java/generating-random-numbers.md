---
title:                "임의의 숫자 생성"
html_title:           "Java: 임의의 숫자 생성"
simple_title:         "임의의 숫자 생성"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성하는 것에 참여하는 이유는 모의 시뮬레이션이나 게임 등 다양한 분야에서 랜덤성을 가지는 데이터가 필요하기 때문입니다.

## 어떻게

```Java
import java.util.Random;

class RandomNumberGenerator {
  public static void main(String[] args) {
    // 랜덤 숫자 생성을 위해 Random 클래스의 객체를 만듭니다.
    Random random = new Random();
    
    // nextInt() 메소드를 사용하여 정수 형태의 랜덤 숫자를 생성합니다.
    int randomNumber = random.nextInt();
    
    System.out.println("생성된 랜덤 숫자: " + randomNumber);
  }
}
```
```
출력:
생성된 랜덤 숫자: 348090243
```

## 심층 탐구

랜덤 숫자를 생성하는 가장 일반적인 방법은 난수 생성기를 사용하는 것입니다. 이는 시드(seed) 값으로부터 난수를 생성하는 알고리즘을 사용하여 랜덤성을 보장합니다. 자바에서는 java.util.Random 클래스를 사용하여 랜덤 숫자를 생성할 수 있으며, 다양한 메소드를 통해 다양한 형태의 랜덤 데이터를 생성할 수 있습니다.

## 참고

- [Java: Random Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Generating Random Numbers in Java](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)