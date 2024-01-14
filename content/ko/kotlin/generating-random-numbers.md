---
title:    "Kotlin: 랜덤 숫자 생성하기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 왜 우리는 난수를 생성하는 일을 해야 할까요?
난수는 컴퓨터 프로그래밍에서 매우 중요한 개념입니다. 난수를 생성함으로써 우리는 충분한 랜덤성을 가진 프로그램을 만들 수 있으며 예측할 수 없는 결과를 얻을 수 있습니다. 또한 많은 게임이나 통계 분석 등에서도 난수 생성이 필수적입니다.

## 방법
### 1. Random 클래스 이용하기
```Kotlin
// 0부터 10 사이의 난수 생성
val random = Random()
val randomNumber = random.nextInt(11)
println(randomNumber) // 출력 결과: 4

// 범위를 지정하여 난수 생성
val random = Random()
val randomNumber = random.nextInt(5, 10)
println(randomNumber) // 출력 결과: 7
```

### 2. Kotlin Random API 이용하기
```Kotlin
// 0부터 10 사이의 난수 생성
val randomNumber = Random.nextInt(11)
println(randomNumber) // 출력 결과: 8

// 범위를 지정하여 난수 생성
val randomNumber = Random.nextInt(5, 10)
println(randomNumber) // 출력 결과: 9
```

### 3. Math.random() 메소드 이용하기
```Kotlin
// 0부터 1 사이의 난수 생성
val randomNumber = Math.random()
println(randomNumber) // 출력 결과: 0.7030286739391777

// 범위를 지정하여 난수 생성
val randomBetweenTenAndTwenty = (10..20).random()
println(randomBetweenTenAndTwenty) // 출력 결과: 17
```

## 더 깊게 살펴보기
난수 생성은 주로 컴퓨터에서 무작위성을 만들기 위해 사용됩니다. 그러나 컴퓨터는 사실 무작위한 값을 생성하기 어렵기 때문에 이러한 난수 생성 방법은 공식적으로는 유사 난수라고 부릅니다. 또한 난수를 생성하는 알고리즘에 따라서 결과 값이 달라질 수 있기 때문에 조금씩 다른 방법들을 적절하게 조합하여 사용하는 것이 좋습니다.

## 연관된 링크들
- [Kotlin Random API 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [Java 버전의 Random 클래스 설명](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java 버전의 Math.random() 메소드 설명](https://docs.oracle.com/javase/7/docs/api/java/lang/Math.html#random())