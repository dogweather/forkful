---
date: 2024-01-27 20:32:50.380635-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB294\uAC00: C++\uC5D0\uC11C \uB09C\uC218\uB97C\
  \ \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uC77C\uBC18\uC801\uC73C\uB85C `<random>`\
  \ \uD5E4\uB354\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\uAC83\uC740 C++11\uC5D0\
  \uC11C \uC18C\uAC1C\uB418\uC5C8\uC73C\uBA70, \uB2E4\uC591\uD55C \uBD84\uD3EC\uC5D0\
  \uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD55C \uAD11\uBC94\uC704\
  \uD55C \uC2DC\uC124\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.662404-06:00'
model: gpt-4-0125-preview
summary: "C++\uC5D0\uC11C \uB09C\uC218\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD574\
  \ \uC77C\uBC18\uC801\uC73C\uB85C `<random>` \uD5E4\uB354\uB97C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 어떻게 하는가:
C++에서 난수를 생성하기 위해 일반적으로 `<random>` 헤더를 사용합니다. 이것은 C++11에서 소개되었으며, 다양한 분포에서 난수를 생성하기 위한 광범위한 시설을 제공합니다.

```C++
#include <iostream>
#include <random>

int main() {
    // 랜덤 엔진 초기화
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // 범위 [0, 99]를 포함하여 정의
    std::uniform_int_distribution<> distrib(0, 99); 

    // 정의된 범위 내에서 5개의 난수 생성 및 출력
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

이 코드 샘플은 `std::random_device`에서 시드를 사용하여 메르센 트위스터 난수 생성기를 초기화합니다. 그런 다음 [0, 99] 범위에서 균일한 정수 분포를 정의하고 마지막으로 이 분포에서 5개의 난수를 출력합니다.

샘플 출력은 다음과 같아 보일 수 있지만, 실행마다 다른 결과를 생성할 가능성이 높습니다:

```
45 67 32 23 88
```

## 심층 탐구:
역사적으로, C++에서의 난수 생성은 `<cstdlib>` 헤더에서 찾을 수 있는 `rand()` 함수와 시드 생성을 위한 `srand()` 함수에 크게 의존했습니다. 그러나, 이 접근법은 생성된 숫자들의 분포에서 균일성과 예측 가능성의 부족으로 종종 비판을 받았습니다.

C++11에서 `<random>` 헤더의 도입은 상당한 개선을 표시했으며, 난수를 생성하기 위한 정교한 시스템을 제공합니다. 제공되는 시설에는 다양한 엔진(예를 들어 메르센 트위스터용 `std::mt19937`)과 분포(예를 들어 정수의 균일 분포용 `std::uniform_int_distribution`)가 포함되어 있으며, 프로그래머의 특정 요구에 맞게 조합될 수 있어 더 예측 가능한 동작, 더 나은 성능, 그리고 더 큰 유연성을 이끌어냅니다.

 `<random>` 라이브러리가 구식 `rand()` 접근법보다 훨씬 나은 것이지만, 특히 암호학적 목적을 위한 진정한 난수 생성은 여전히 추가적인 고려 사항에 의존한다는 점을 언급할 가치가 있습니다. 암호학적 애플리케이션을 위해서는, 종종 하드웨어 엔트로피 소스를 활용하는 보안에 특화된 라이브러리가 대신 사용되어야 합니다.
