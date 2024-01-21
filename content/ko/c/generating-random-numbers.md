---
title:                "난수 생성하기"
date:                  2024-01-20T17:48:32.449606-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자 생성은 예측할 수 없는 수를 만드는 과정입니다. 프로그래머들은 게임, 시뮬레이션, 보안 알고리즘 등에서 불확실성을 도입하기 위해 이를 사용합니다.

## How to: (방법)
C에서 랜덤 숫자를 생성하기 위해서는 `stdlib.h` 헤더 파일의 `rand()` 함수를 사용합니다. 예를 들어, 0에서 99 사이의 랜덤 숫자를 5개 생성하는 코드입니다:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // 난수 생성기 초기화
    srand((unsigned int)time(NULL));
    
    for(int i = 0; i < 5; i++) {
        int randomNumber = rand() % 100; // 0에서 99 사이의 수
        printf("%d\n", randomNumber);
    }
    
    return 0;
}
```

실행 결과 예시:

```
42
76
13
7
59
```

## Deep Dive (심층 분석)
`rand()` 함수는 난수를 생성하지만, 실제로는 의사 난수 생성기(Pseudo-Random Number Generator, PRNG)입니다. 씨앗(seed) 값에 따라 생성되는 숫자의 순서가 결정되는데, `srand()` 함수를 사용하여 씨앗을 설정합니다. 일반적으로 현재 시간을 씨앗으로 사용하여 각 실행마다 다른 결과를 얻습니다.

C99 이전에는 `rand()`와 `srand()`가 표준이었으나 최신 C 표준에서는 추가적인 난수 생성 함수들을 도입했습니다. `<random.h>` 헤더 파일 안에 있는 `random()`와 `srandom()` 함수는 더 큰 범위와 더 좋은 난수 특성을 가지고 있습니다. 아직도 많은 시스템에서 `rand()`는 사용되지만, 보안이 중요한 애플리케이션에서는 정보보안을 위해 더 강력한 난수 생성기가 필요합니다.

## See Also (추가 정보)
- [`rand` function reference from cppreference.com](https://en.cppreference.com/w/c/numeric/random/rand)
- [ISO C documentation for more details on random number generation](https://www.iso.org/standard/74528.html)