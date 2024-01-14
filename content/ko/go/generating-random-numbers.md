---
title:    "Go: 난수 생성하기"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜
랜덤한 숫자를 생성하는 일에 참여하는 이유는 다양합니다. 예를 들어서, 게임 개발이나 무작위 배정 문제를 해결하기 위해 랜덤한 숫자가 필요할 수 있습니다. 또는 새로운 알고리즘을 테스트하는 등 다양한 상황에서 랜덤한 숫자 생성이 필요할 수 있습니다. 따라서 Go 언어에서 제공하는 랜덤 함수를 알아보고 활용하는 방법을 소개하겠습니다.

# 사용 방법
랜덤한 숫자를 생성하기 위해서는 Go 언어에서 제공하는 `math/rand` 패키지를 사용해야 합니다. 이 패키지에는 여러 가지 랜덤 함수들이 포함되어 있으며, 각 함수에 대한 설명과 예제를 아래의 코드 블록에 제공하겠습니다.

```Go
// 정수형 랜덤한 숫자 생성
rand.Intn(100) // 0 이상 99 이하의 정수 생성
rand.Int63n(10) // 0 이상 9 이하의 int64 정수 생성
rand.Int31n(5) // 0 이상 4 이하의 int32 정수 생성

// 실수형 랜덤한 숫자 생성
rand.Float64() // 0 이상 1 미만의 실수 생성
rand.NormFloat64() // 평균 0, 표준편차 1인 정규분포를 따르는 실수 생성

// 랜덤한 순서로 배열 섞기
numbers := []int{1, 2, 3, 4, 5}
rand.Shuffle(len(numbers), func(i, j int) {
	numbers[i], numbers[j] = numbers[j], numbers[i]
})
// numbers의 값이 랜덤한 순서로 섞인 상태로 출력됩니다.
fmt.Println(numbers)
```

위의 코드에서는 `import "math/rand"`를 통해 `math/rand` 패키지를 사용하고 있습니다. 또한 `math/rand` 패키지는 시드(seed)를 설정해주지 않으면 실행할 때마다 같은 랜덤한 값을 출력하기 때문에, 보다 랜덤하게 값을 출력하기 위해 `rand.Seed(time.Now().UnixNano())` 코드를 추가해주는 것이 좋습니다.

# 깊이 파고들기
Go 언어에서 제공하는 랜덤 함수들은 여러 알고리즘을 통해 랜덤한 값을 생성합니다. 대표적인 알고리즘으로는 선형 합동법(Linear Congruential Generator)과 Mersenne Twister가 있습니다. 각 알고리즘마다 주기(Period)와 적합성(Test of randomness) 등 다양한 특징이 있으며, 필요에 따라 적합한 알고리즘을 선택해서 사용할 수 있습니다.

또한 랜덤 함수를 사용할 때 주의해야 할 점으로는 암호학적 용도로는 사용되지 않는다는 것입니다. 이는 랜덤 함수를 통해 생성된 값들은 예측이 가능하기 때문입니다. 따라서 암호화처리 등 보안에 민감한 경우에는 별도의 암호학적으로 안전한 난수 발생기(Cryptographically Secure Pseudo-Random Number Generator)를 사용해야 합니다.

# 참고자료
- [Go 언어 공식 문서 - math/rand 패키지](https://golang.org/pkg/math/rand/)
- [랜덤 함수에 대한 더 자세한 설명](https://en.wikipedia.org/wiki/Random_number_generation)
- [암호학적으로 안전한