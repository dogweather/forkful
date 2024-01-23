---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:44.965904-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
랜덤 숫자 생성은 예측할 수 없는 숫자를 만드는 거예요. 프로그래머들은 게임, 보안, 시뮬레이션 등에서 현실성을 주거나 테스트 데이터를 만들기 위해 사용해요.

## How to: (방법)
```C++
#include <iostream>
#include <random>

int main() {
    // 난수 발생기 초기화
    std::random_device rd;
    std::mt19937 gen(rd());
    
    // 범위 정의
    std::uniform_int_distribution<> distrib(1, 100);

    // 랜덤 숫자 생성 및 출력
    for(int n=0; n<10; ++n)
        std::cout << distrib(gen) << ' ';
    
    return 0;
}
```

**샘플 출력:**
```
42 76 13 28 92 35 79 11 67 53
```

## Deep Dive (심층 분석)
과거에는 `rand()`와 `srand()` 함수로 랜덤 숫자를 생성했지만, 예측 가능할 수 있어 현대 표준에서는 `<random>` 헤더의 클래스들을 사용하는 것을 권장해요. 예를 들어, `std::mt19937`은 머신 트위스터 알고리즘을 사용하면서 좋은 품질의 난수를 제공해요. 배열, 벡터 등 다량의 데이터에 랜덤 요소를 적용할 때 효과적이죠.

`<random>` 헤더는 난수 생성 엔진(`std::random_device`, `std::mt19937` 등)과 분포(`std::uniform_int_distribution<>`, `std::normal_distribution<>` 등)를 제공해요. 분포를 사용하면 특정 범위 내에서 균등하게, 또는 정규분포와 같은 특정 방식으로 난수를 생성할 수 있어요.

난수 생성의 구현 세부사항을 살펴보면, `std::random_device`는 비결정적 난수를 생성할 수 있는 시스템의 엔트로피 소스에 기반해요. 그런데 모든 시스템이 비결정적 난수를 제공하지 않으니 `std::mt19937` 엔진으로 초기화를 한 다음 난수를 생성하는 것이 더 일반적이에요. 

## See Also (참고 자료)
1. [cppreference.com - <random>](https://en.cppreference.com/w/cpp/header/random): C++의 난수 생성에 대한 표준 라이브러리 설명입니다.
2. [cplusplus.com - Random number distribution](http://www.cplusplus.com/reference/random/): 다양한 난수 분포 유형에 대한 정보입니다.
3. [Wikipedia - Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister): 머신 트위스터 알고리즘에 대한 자세한 정보입니다.
