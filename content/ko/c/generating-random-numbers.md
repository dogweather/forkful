---
title:                "난수 생성"
date:                  2024-01-27T20:33:13.839478-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

C에서 난수를 생성하는 것은 어떤 패턴도 식별할 수 없는 숫자의 열을 만드는 것을 포함하며, 무작위성의 개념을 모방합니다. 프로그래머들은 데이터 시뮬레이션, 암호학적 응용, 게임 개발 등 다양한 목적으로 난수를 활용하며, 이는 프로그래밍의 중요한 측면입니다.

## 어떻게 할까?

C에서 난수를 생성하려면 `stdlib.h`에 있는 `rand()` 함수를 주로 사용합니다. 그러나 프로그램 실행 간에 생성된 숫자의 변별성을 보장하기 위해 난수 생성기를 시드(seed)하는 것이 중요합니다. 현재 시간과 같은 값을 시드로 사용하는 `srand()` 함수가 이를 용이하게 합니다.

0부터 99 사이의 난수를 생성하는 간단한 예는 다음과 같습니다:
```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 난수 생성기 시드하기
    srand((unsigned) time(NULL));

    // 0과 99 사이의 난수 생성하기
    int randomNumber = rand() % 100;

    // 난수 출력하기
    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

예시 출력:

```
Random Number: 42
```

현재 시간으로 시드하기 때문에 이 프로그램의 각 실행은 새로운 난수를 생성할 것임을 알아두는 것이 중요합니다.

## 심층 분석

`rand()`와 `srand()`를 사용하여 C에서 난수를 생성하는 전통적인 방법은 진정으로 무작위가 아닙니다. 이는 의사 난수입니다. 많은 응용 프로그램에서는 이것이 괜찮지만, 심각한 암호학적 사용과 같이 높은 수준의 무작위성이 필요한 상황에서는 부족합니다. `rand()`에 의해 생성된 수열은 `srand()`에 제공된 시드에 의해 완전히 결정됩니다. 따라서 시드가 알려져 있다면, 수열을 예측할 수 있어 무작위성이 감소합니다.

역사적으로, `rand()` 함수는 그 낮은 질의 무작위성과 제한된 범위로 인해 비판을 받았습니다. 현대적인 대안은 장치별 API를 사용하거나 진정한 무작위성을 더 잘 근사하는 외부 라이브러리를 사용하거나, UNIX 계열 시스템에서는 암호학적 목적으로 `/dev/random` 또는 `/dev/urandom`에서 읽는 것을 포함합니다.

예를 들어, C에서 `/dev/urandom`을 사용하는 방법:
```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // 읽기 위해 /dev/urandom 열기
    fp = fopen("/dev/urandom", "r");

    // 무작위 수읽기
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // 무작위 수 출력하기
    printf("Random Number: %u\n", randomNumber);

    // 파일 닫기
    fclose(fp);

    return 0;
}
```

이 방법은 시스템의 엔트로피 풀에서 직접 읽어 보다 높은 질의 무작위성을 제공하는데, 이는 더 민감한 응용 프로그램에 적합합니다. 그러나 이 접근 방식은 다양한 플랫폼 간의 이식성 문제를 가질 수 있어 `rand()`를 사용하는 것보다 덜 보편적일 수 있습니다.

방법에 관계없이 무작위성의 본질과 C에서의 구현을 이해하는 것은 효과적이고 안전하며 매력적인 응용 프로그램을 개발하는 데 중요합니다.
