---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 넘버 생성은 예측할 수 없는 수를 생성하는 프로세스입니다. 프로그래머들은 이를 통해 데이터 보안, 암호 생성, 게임 및 시뮬레이션에서의 불규칙성을 구현합니다.

## 어떻게:

다음은 Java에서 랜덤 넘버를 생성하는 방법입니다:

```Java
import java.util.Random;

public class Main {
  public static void main(String[] args) {
    // Random 객체를 생성
    Random rand = new Random();

    // 0보다 크거나 같고 50보다 작은 정수를 생성
    int num = rand.nextInt(50); 
    System.out.println("생성된 랜덤 넘버: " + num);
  }
}
```
이 코드를 실행 시키면 다음과 같은 결과가 나옵니다:

```
생성된 랜덤 넘버: 37
```

## 알아두기:

Java에서의 랜덤 넘버 생성은 오랫동안 `java.util.Random` 클래스를 사용하여 처리되어 왔습니다. 그러나 Java 1.7 이후부터는 `java.util.concurrent.ThreadLocalRandom` 클래스를 사용하는 것이 더 좋은 선택입니다. 이 클래스는 다중 스레드 환경에서의 랜덤 넘버 생성을 더 효율적으로 처리합니다.

대안으로 `Math.random()` 메소드도 사용할 수 있습니다. 이 메소드는 0.0에서 1.0 사이의 double 값으로 랜덤 넘버를 반환합니다. 하지만 주어진 범위 내의 정수 랜덤 넘버를 생성할 때에는 `Random` 또는 `ThreadLocalRandom` 클래스가 더 유용합니다.

## 참고 링크:

- [Java의 Random 클래스 공식 문서](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/Random.html)
- [Java의 ThreadLocalRandom 클래스 공식 문서](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html)
- [Java의 Math 클래스 공식 문서](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/Math.html)