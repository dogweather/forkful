---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 숫자 생성이란 무작위의 숫자를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 머신 러닝 등에서 예측 불가능한 결과를 만들기 위해 이를 활용합니다.

## 어떻게:

방법은 매우 간단합니다. C 언어에서는 `rand()` 함수를 사용하여 랜덤 숫자를 생성할 수 있습니다. 다음은 예제 코드입니다:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0)); 
    printf("Random Number: %d\n", rand()%100); 
    return 0;
}
```

이 코드는 0부터 99 사이의 랜덤 숫자를 생성합니다. 출력은 매번 다릅니다.

## 깊게 알아보기:

랜덤 숫자 생성은 컴퓨터과학의 중요한 분야입니다. 초기 컴퓨터 시대부터 랜덤성은 암호학, 시뮬레이션, 테스트 케이스 생성 등에 쓰였습니다.

`rand()` 함수 외에도 다른 생성 방법들도 존재합니다. 예를 들어, cryptographically secure pseudo-random number generator (CSPRNG) 같은 더 강력한 방법들이 있습니다.

레퍼런스를 위해, `rand()` 함수는 실제로는 pseudo-random number generator (PRNG)를 사용합니다. 즉, 초깃값(seed)에 따라 랜덤처럼 보이는 숫자를 생성하는 함수입니다.

## 참고 자료:

- C 언어의 `rand()` 함수에 대한 더욱 자세한 설명: [https://www.cplusplus.com/reference/cstdlib/rand/](https://www.cplusplus.com/reference/cstdlib/rand/)
- 랜덤 숫자 생성에 대한 아주 깊고 알찬 내용: [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
- Cryptographically secure pseudo-random number generator (CSPRNG)에 대한 정보: [https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)