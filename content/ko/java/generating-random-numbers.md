---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:53.462191-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

랜덤 숫자 생성은 예측할 수 없는 숫자를 만드는 거예요. 이것은 게임, 시뮬레이션, 보안 시스템 등 다양한 프로그램에서 필수적인데, 이를 통해 실제와 가까운 상황을 만들거나 데이터를 보호할 수 있어요.

## How to: (방법:)

자바에서 랜덤 숫자를 생성하는 것은 간단해요. `Random` 클래스나 `Math.random()`을 이용하면 되죠. 아래는 예시 코드에요.

```java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        // Random 클래스 사용
        Random random = new Random();
        int randomInt = random.nextInt(100); // 0부터 99까지 랜덤 정수
        double randomDouble = random.nextDouble(); // 0.0과 1.0 사이 랜덤 실수
        System.out.println("랜덤 정수: " + randomInt);
        System.out.println("랜덤 실수: " + randomDouble);

        // Math.random() 사용
        double mathRandom = Math.random(); // 0.0과 1.0 사이 랜덤 실수
        System.out.println("Math.random() 실수: " + mathRandom);
    }
}
```

실행 결과는 예시이며, 매번 다를 거예요.
```
랜덤 정수: 42
랜덤 실수: 0.53907
Math.random() 실수: 0.12345
```

## Deep Dive (심층 분석):

랜덤 숫자 생성은 오래전부터 컴퓨터 과학에서 중요한 부분이었어요. 이론상, 컴퓨터는 완벽한 랜덤을 생성할 수 없기 때문에 사용하는 것은 '의사(Pseudo) 랜덤'이라고 불러요. 즉, 알고리즘에 따라 생성되는데, 잘 만들어진 알고리즘이면 결과는 마치 랜덤처럼 보일 거예요.

다른 방법으로는 `SecureRandom` 클래스가 있어요. 이것은 암호학적으로 강력한 랜덤 숫자를 필요로 할 때 사용해요.

자바의 `Random` 클래스는 선형 합동 방식을 사용해요. 이 방식은 초기 '시드(seed)' 값을 기반으로 연산을 반복해서 랜덤 수를 생성하는데, 동일한 시드 값에서는 동일한 수열이 나와요.

## See Also (추가자료):

- Java API Documentation on Random: [Oracle Random Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html)
- Oracle guide to SecureRandom: [Oracle SecureRandom Guide](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- Wikipedia on Random Number Generation: [Random Number Generation](https://en.wikipedia.org/wiki/Random_number_generation)
