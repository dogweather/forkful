---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:40.394866-07:00
description: "C\uC5D0\uC11C \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD558\uB294\
  \ \uAC83\uC740 \uC608\uCE21\uD560 \uC218 \uC5C6\uACE0 \uD2B9\uC815 \uBD84\uD3EC\
  (\uC608: \uADE0\uB4F1 \uB610\uB294 \uC815\uADDC)\uB97C \uB530\uB974\uB294 \uAC12\
  \uB4E4\uC744 \uB9CC\uB4DC\uB294 \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uC774 \uAE30\uB2A5\uC740 \uC608\uCE21\uD560 \uC218 \uC5C6\uC74C \uB610\uB294\
  \ \uC2E4\uC138\uACC4\uC758 \uBB34\uC791\uC704\uC131\uC744 \uBAA8\uBC29\uD558\uB294\
  \ \uAC83\uC774 \uD544\uC218\uC801\uC778 \uC2DC\uBBAC\uB808\uC774\uC158\uACFC \uAC8C\
  \uC784\uC73C\uB85C\uBD80\uD130 \uC554\uD638\uD654 \uC791\uC5C5\uC5D0 \uC774\uB974\
  \uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\
  \u2026"
lastmod: '2024-03-13T22:44:55.916215-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\
  \uC740 \uC608\uCE21\uD560 \uC218 \uC5C6\uACE0 \uD2B9\uC815 \uBD84\uD3EC(\uC608:\
  \ \uADE0\uB4F1 \uB610\uB294 \uC815\uADDC)\uB97C \uB530\uB974\uB294 \uAC12\uB4E4\uC744\
  \ \uB9CC\uB4DC\uB294 \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "\uB79C\uB364 \uC22B\uC790 \uC0DD\uC131\uD558\uAE30"
weight: 12
---

## 방법:
C에서는 표준 라이브러리 `<stdlib.h>`에 속한 `rand()` 함수를 사용하여 무작위 수를 생성할 수 있습니다. 기본적으로, `rand()`는 0부터 `RAND_MAX`( `<stdlib.h>`에서 정의된 상수) 범위의 의사 무작위 수를 생성합니다. 범위를 더 제어하려면 프로그래머가 `rand()`의 출력을 조작할 수 있습니다.

0에서 99 사이의 무작위 수를 생성하는 간단한 예는 다음과 같습니다:

```c
#include <stdio.h>
#include <stdlib.h> // rand() 및 srand() 용
#include <time.h>   // time() 용

int main() {
    // 무작위 수 생성기 시드 설정
    srand((unsigned) time(NULL));

    // 0에서 99 사이의 무작위 수 생성
    int randomNumber = rand() % 100;

    printf("무작위 수: %d\n", randomNumber);

    return 0;
}
```

이 프로그램을 실행할 때마다 샘플 출력은 다르게 나올 수 있습니다:

```
무작위 수: 42
```
다른 범위 내에서 무작위 수를 생성하려면, 모듈러스 연산자(`%`)를 그에 맞게 조정할 수 있습니다. 예로, `rand() % 10`은 0에서 9까지의 숫자를 생성합니다.

프로그램 실행 간에 다른 무작위 수 시퀀스를 보장하기 위해 현재 시간(`time(NULL)`)으로 의사 난수 생성기(`srand()` 호출)를 시드하는 것이 중요합니다. 시드 설정(`srand()`) 없이 `rand()`는 프로그램이 실행될 때마다 같은 수의 시퀀스를 생성할 것입니다.

## 심층 분석
`rand()` 함수와 그 시드 설정 카운터파트인 `srand()`는 수십 년 동안 C 표준 라이브러리의 일부였습니다. 이것들은 무작위로만 보이는 수의 시퀀스를 생성하는 알고리즘에 기초하고 있습니다—그래서 "의사-무작위"라는 용어가 사용됩니다. `rand()`의 기본적인 알고리즘은 일반적으로 선형 합동 생성기(LCG)입니다.

`rand()`와 `srand()`는 많은 애플리케이션에 충분하지만, 특히 무작위성의 품질과 예측 가능성과 관련하여 알려진 한계가 있습니다. 암호 작업과 같이 고품질의 무작위성이 필요한 애플리케이션의 경우, 유닉스 계열 시스템에서의 `/dev/random`이나 `/dev/urandom`, 또는 암호화 라이브러리가 제공하는 API와 같은 대체재를 고려해야 합니다.

C11의 도입과 함께, ISO C 표준은 동시성 작업에 대해 더 정밀한 제어를 제공하는 새로운 헤더인 `<stdatomic.h>`를 포함했지만, 무작위성과는 직접적으로 관련되지 않았습니다. C에서 진정한 무작위성을 원하는 개발자들은 종종 더 나은 알고리즘을 제공하거나 하드웨어 엔트로피 소스를 사용하는 플랫폼별 또는 외부 라이브러리로 전환합니다.

`rand()`는 의사 무작위 수를 생성하는 간단하고 접근하기 쉬운 수단으로서 기능하지만, 그 출력의 품질과 예측 가능성에 의해 현대 애플리케이션에서 사용되는 범위가 제한됩니다. 보안을 의식하는 애플리케이션과 같이 더 견고한 해결책이 필요한 경우, 표준 라이브러리를 넘어서 탐색하는 것이 강력히 권장됩니다.
