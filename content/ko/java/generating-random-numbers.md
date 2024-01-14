---
title:    "Java: 랜덤 숫자 생성하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜 : 난수 생성에 참여하는 이유

난수 생성은 Java 프로그래밍에서 매우 유용합니다. 예를 들어, 게임에서 무작위적으로 이벤트를 발생시키거나, 시뮬레이션에서 랜덤한 상황을 만들거나, 테스트를 위해 무작위 데이터를 생성하는 등 다양한 상황에서 사용될 수 있습니다. 따라서 난수 생성은 프로그래머에게 매우 중요한 기능입니다.

# 어떻게 : 난수 생성하는 방법

Java에서 난수를 생성하는 가장 간단한 방법은 Math 클래스의 random() 메소드를 사용하는 것입니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Java
double randomNum = Math.random(); // 0부터 1 사이의 난수 생성
System.out.println(randomNum); // 콘솔에 결과 출력
```

위의 코드를 실행하면, 0부터 1 사이의 무작위한 숫자가 출력될 것입니다. 하지만 이 메소드는 실제로는 0~1 사이의 double 형 난수를 생성해주는 것이 아니라, 0 이상 1 미만의 double 형 난수를 생성해줍니다. 따라서 만약 우리가 1부터 10 사이의 정수 난수를 생성하고 싶다면 다음과 같이 코드를 수정해주어야 합니다.

```Java
int randomNum = (int) (Math.random() * 10) + 1; // 1부터 10 사이의 정수 난수 생성
System.out.println(randomNum); // 콘솔에 결과 출력
```

이 외에도 Random 클래스를 사용해 난수를 생성하거나, SecureRandom 클래스를 사용해 더 안전한 난수를 생성하는 방법도 있습니다. 하지만 위에서 언급한 방법이 가장 간단하고 자주 사용되는 방법입니다.

# 자세히 살펴보기 : 난수 생성의 깊은 이해

Random 클래스를 사용해 난수를 생성할 때, 기본적으로는 seed 값을 사용합니다. seed 값에 따라 생성되는 난수의 순서와 패턴이 결정됩니다. 따라서 같은 seed 값이면 항상 같은 순서와 패턴의 난수를 생성할 수 있습니다.

하지만 SecureRandom 클래스는 보안적인 이유로 seed 값을 무작위적으로 생성해줍니다. 이는 누군가 seed 값을 알아내어 예측할 수 없도록 만들어주는 역할을 합니다.

또한 Random 클래스에서는 nextInt(), nextDouble() 등의 메소드를 사용해 난수를 생성하는 것이 가능하지만, SecureRandom 클래스에서는 이렇게 되지 않습니다. SecureRandom 클래스를 사용할 때는 밀리세컨드, 바이트 배열 등 다양한 파라미터를 사용해 난수를 생성할 수 있습니다.

## 참고자료

- [Oracle Java Documentation - Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Oracle Java Documentation - SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)

# 관련 자료

- [Java의 수학 함수들 - Math 클래스](https://www.edwith.org/java-web)
- [Java의 Random 클래스는 어떻게 작동할까](https://www.inflearn.com/course/Java-tdd/lecture/22894)