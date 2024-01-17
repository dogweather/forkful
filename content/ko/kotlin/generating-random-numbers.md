---
title:                "랜덤 수 생성하기"
html_title:           "Kotlin: 랜덤 수 생성하기"
simple_title:         "랜덤 수 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

난수 생성이란 무엇일까요? 우리는 우리의 프로그램이 정해진 범위 내의 임의의 수를 선택할 수 있게끔 만드는 것입니다. 프로그래머들이 이를 할 때는 예측할 수 없는 값이 필요할 때 사용됩니다. 예를 들어 가위, 바위, 보 게임에서 우리는 컴퓨터가 무엇을 내서 우리가 이길지 판단할 수 없기 때문에 난수 생성을 사용합니다.

## 사용법:

우리는 매우 쉽게 Kotlin에서 난수를 생성할 수 있습니다. 아래의 예제 코드를 살펴보세요. 

```Kotlin
// 1에서부터 10 사이의 숫자 중 무작위로 하나를 선택합니다.
val randomNum = (1..10).random()

// 0부터 10 사이의 숫자 중 무작위로 하나를 선택합니다.
val randomNumZero = (0..10).random()

// 사용자가 정한 범위 내의 무작위 숫자를 선택합니다.
val randomNumUser = (start..end).random()
```

위의 예제 코드에서는 ```random()``` 메서드를 사용해서 난수를 생성하고 있습니다. 이 메서드는 Int 클래스에 소속되어 있습니다. 따라서 우리는 Int 변수를 만들어서 메서드를 호출하면 됩니다. 또한 우리는 필요한 범위를 지정하고 ```..``` 연산자를 사용하여 난수를 생성할 수 있습니다.

## 깊이있게 알아보기:

난수 생성에 대해 더 깊이있게 알아볼까요? 난수 생성은 프로그래밍의 초창기부터 사용되어 왔습니다. 이전에는 우리가 사용하던 컴퓨터들은 매우 예측할 수 있는 저수준의 프로그래밍 언어를 사용하고 있었기 때문에 예측 가능한 값만 생성할 수 있었습니다. 따라서 난수 생성은 우리에게 예측할 수 없는 값이 필요할 때 이용하고 있었습니다. 또한, 다른 난수 생성 방식에는 여러가지가 있습니다. 예를 들어 우리는 의사 난수 생성기를 사용하고 있습니다.

## 더 많이 알아보기:

- [Kotlin 표준 라이브러리 문서: random() 메서드](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.ranges/random.html)
- [난수 생성의 역사](https://ieeexplore.ieee.org/document/1528970)
- [다른 난수 생성 방법에 대해](https://www.javaworld.com/article/2077564/computing-ai/using-seedwithnewsnexception-coming-in-jdk-5-0.html) 보다 자세히 알아보세요.