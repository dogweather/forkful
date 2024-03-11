---
date: 2024-01-27 20:32:50.380635-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\
  \uC740 \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC21C\uC11C\uB098 \uD328\uD134\uC774\
  \ \uC5C6\uB294 \uC22B\uC790 \uC2DC\uD000\uC2A4\uB97C \uB9CC\uB4DC\uB294 \uAC83\uC744\
  \ \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\
  \uB7EC\uD55C \uC22B\uC790\uB4E4\uC744 \uC885\uC885 \uC608\uCE21\uD560 \uC218 \uC5C6\
  \uB294 \uC774\uBCA4\uD2B8\uB97C \uC2DC\uBBAC\uB808\uC774\uC158\uD558\uB294 \uB370\
  , \uD14C\uC2A4\uD2B8 \uBC0F \uB514\uBC84\uAE45\uC5D0, \uADF8\uB9AC\uACE0 \uACF5\uC815\
  \uC131 \uD639\uC740 \uC608\uCE21 \uBD88\uAC00\uB2A5\uC131\uC744 \uBCF4\uC7A5\uD558\
  \uAE30 \uC704\uD55C \uAC8C\uC784 \uC54C\uACE0\uB9AC\uC998\uC5D0 \uB2E4\uC591\uD55C\
  \u2026"
lastmod: '2024-03-11T00:14:29.588849-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740\
  \ \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC21C\uC11C\uB098 \uD328\uD134\uC774 \uC5C6\
  \uB294 \uC22B\uC790 \uC2DC\uD000\uC2A4\uB97C \uB9CC\uB4DC\uB294 \uAC83\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB7EC\
  \uD55C \uC22B\uC790\uB4E4\uC744 \uC885\uC885 \uC608\uCE21\uD560 \uC218 \uC5C6\uB294\
  \ \uC774\uBCA4\uD2B8\uB97C \uC2DC\uBBAC\uB808\uC774\uC158\uD558\uB294 \uB370, \uD14C\
  \uC2A4\uD2B8 \uBC0F \uB514\uBC84\uAE45\uC5D0, \uADF8\uB9AC\uACE0 \uACF5\uC815\uC131\
  \ \uD639\uC740 \uC608\uCE21 \uBD88\uAC00\uB2A5\uC131\uC744 \uBCF4\uC7A5\uD558\uAE30\
  \ \uC704\uD55C \uAC8C\uC784 \uC54C\uACE0\uB9AC\uC998\uC5D0 \uB2E4\uC591\uD55C\u2026"
title: "\uB09C\uC218 \uC0DD\uC131"
---

{{< edit_this_page >}}

## 무엇이며 왜?

프로그래밍에서 난수 생성은 예측할 수 없는 순서나 패턴이 없는 숫자 시퀀스를 만드는 것을 포함합니다. 프로그래머들은 이러한 숫자들을 종종 예측할 수 없는 이벤트를 시뮬레이션하는 데, 테스트 및 디버깅에, 그리고 공정성 혹은 예측 불가능성을 보장하기 위한 게임 알고리즘에 다양한 용도로 활용합니다.

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
