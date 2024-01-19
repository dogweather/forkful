---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 숫자 생성은 예측 불가능한 숫자를 만드는 것입니다. 프로그래머들이 이를 사용하는 주된 이유는 시뮬레이션, 게임, 테스트, 보안 등의 다양한 상황에서 비결정적인 행동이 필요할 때입니다.

## 어떻게 해야 할까?

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 100);
    std::cout << "랜덤 숫자: " << distr(gen) << '\n';
    return 0;
}
```
이 코드를 실행하면 1부터 100 사이의 랜덤 숫자를 출력합니다.

## 깊은 탐구

랜덤 숫자 생성은 컴퓨터 과학이 발전하면서 매우 중요해졌습니다. 초기에는 순차적이거나 고정 패턴을 가진 수열을 "랜덤"으로 사용하였지만 이는 진정한 랜덤성이 결여되어 있었습니다. 이런 이유로 우리는 Mersenne Twister 같은 알고리즘 (여기서 사용된 `std::mt19937`)을 도입하게 되었고, 이는 사실적인 '랜덤' 수를 더 잘 생성합니다.

이외에도 C++에서는 `<random>` 라이브러리를 이용해 다양한 랜덤 숫자 생성기를 사용할 수 있습니다.

## 관련 자료

더 많은 정보를 얻기 위해 다음 링크를 참조하세요.

- (C++ 레퍼런스) [`<random>`](http://www.cplusplus.com/reference/random/) 
- (학술 논문) ["Mersenne Twister: A 623-dimensionally equidistributed uniform pseudo-random number generator"](https://dl.acm.org/doi/10.1145/272991.272995)
- (튜토리얼) ["Random Numbers in C++"](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)