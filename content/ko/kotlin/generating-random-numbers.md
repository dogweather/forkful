---
title:                "Kotlin: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자를 생성하는 것에 참여하는 이유는 무엇일까요? 누구나 새로운 프로그래밍 언어를 배우기 위해서는 여러 가지 기술을 연습하는 것이 중요합니다. 또한 새로운 기능이나 알고리즘을 구현할 때에는 종종 랜덤 숫자가 필요하므로 이를 생성하는 방법을 알고 있는 것이 중요합니다.

## 하우 투
랜덤 숫자를 생성하는 가장 간단한 방법은 ```Random``` 클래스를 사용하는 것입니다. 아래의 코드를 Kotlin 파일에 작성하고 실행해보세요.

```Kotlin
import java.util.Random

fun main() {
    val random = Random()
    println(random.nextInt(10))
}
```

위의 예시에서는 0부터 9까지의 랜덤 숫자를 출력합니다. 이를 변경하고 싶다면 ```nextInt()``` 메소드에 원하는 범위를 지정해주면 됩니다. 예를 들어, ```nextInt(100)```의 경우 0부터 99까지의 랜덤 숫자를 출력하게 됩니다.

만약 인덱스가 0부터 시작하는 배열을 랜덤하게 선택하고 싶다면 아래와 같은 코드를 사용할 수 있습니다.

```Kotlin
val array = arrayOf("사과", "바나나", "딸기", "수박")
val randomIndex = random.nextInt(array.size)
println(array[randomIndex])
```

또는 특정 범위 내에서 실수형의 랜덤 숫자를 생성하고 싶다면 아래의 코드를 사용할 수 있습니다.

```Kotlin
println(random.nextDouble(20.0))
```

이 외에도 랜덤 숫자를 생성하는 다양한 방법이 있으니 자유롭게 찾아보세요!

## 딥 다이브
컴퓨터 프로그래밍에서 랜덤 숫자를 생성하는 방법은 매우 중요합니다. 하지만 그중에서도 균일하고 무작위성을 가지는 숫자를 생성하는 것은 매우 어렵습니다. 그 이유는 컴퓨터는 무조건적으로 계산하는 머신이기 때문입니다.

따라서 좋은 랜덤 숫자를 생성하기 위해서는 컴퓨터의 내부에서 발생하는 무작위성에 대해서도 이해하는 것이 중요합니다. 또한 랜덤 숫자를 생성하는 알고리즘에 대해서도 이해하고 적절한 방식으로 적용하는 것이 좋습니다.

## 이 또한 참고하세요
- [Kotlin 공식 문서](https://kotlinlang.org/docs/basic-syntax.html#random-numbers)
- [Effective Kotlin: Random number generation](https://medium.com/androiddevelopers/effective-kotlin-random-number-generation-90672443fad)
- [How to generate Random numbers in Kotlin](https://android--code.blogspot.com/2018/02/kotlin-random-int-float-double-long-java.html)