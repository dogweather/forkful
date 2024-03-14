---
date: 2024-01-27 20:33:15.259965-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\
  \uC740 \uC0AC\uC804\uC5D0 \uB17C\uB9AC\uC801\uC73C\uB85C \uC608\uCE21\uD560 \uC218\
  \ \uC5C6\uB294 \uAC12\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACE0\uC720 \uC2DD\
  \uBCC4\uC790 \uC0DD\uC131, \uAC8C\uC784 \uAC1C\uBC1C \uC2DC \uC2DC\uBBAC\uB808\uC774\
  \uC158, \uB610\uB294 \uBD84\uC11D\uC744 \uC704\uD55C \uB370\uC774\uD130\uC5D0\uC11C\
  \ \uC784\uC758\uC758 \uC0D8\uD50C \uC120\uD0DD \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\
  \uB85C \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.654412-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740\
  \ \uC0AC\uC804\uC5D0 \uB17C\uB9AC\uC801\uC73C\uB85C \uC608\uCE21\uD560 \uC218 \uC5C6\
  \uB294 \uAC12\uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACE0\uC720 \uC2DD\uBCC4\
  \uC790 \uC0DD\uC131, \uAC8C\uC784 \uAC1C\uBC1C \uC2DC \uC2DC\uBBAC\uB808\uC774\uC158\
  , \uB610\uB294 \uBD84\uC11D\uC744 \uC704\uD55C \uB370\uC774\uD130\uC5D0\uC11C \uC784\
  \uC758\uC758 \uC0D8\uD50C \uC120\uD0DD \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\uB85C\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
---

{{< edit_this_page >}}

## 무엇과 왜?

프로그래밍에서 난수 생성은 사전에 논리적으로 예측할 수 없는 값을 생성하는 것에 대한 것입니다. 프로그래머들은 고유 식별자 생성, 게임 개발 시 시뮬레이션, 또는 분석을 위한 데이터에서 임의의 샘플 선택 등 다양한 이유로 이 작업을 수행합니다.

## 방법:

Clojure에서는 난수 생성이 간단하며, 바로 사용할 수 있는 몇 가지 내장 함수가 있습니다.

0(포함)과 1(제외) 사이의 난수 부동 소수점을 생성하려면 `rand` 함수를 사용할 수 있습니다:

```Clojure
(rand)
;; 예시 출력: 0.7094245047062917
```

특정 범위 내의 정수가 필요한 경우, `rand-int`를 사용하세요:

```Clojure
(rand-int 10)
;; 예시 출력: 7
```

이 함수는 0(포함)과 인자로 전달된 숫자(제외) 사이의 난수 정수를 생성합니다.

특정 범위 내의 난수를 생성하려면 (정수에 한정되지 않음), `rand`와 산술을 결합할 수 있습니다:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; 사용법
(rand-range 10 20)
;; 예시 출력: 14.857457734992847
```

이 함수 `rand-range`는 지정된 `min`과 `max` 값 사이의 난수 부동 소수점을 반환합니다.

반복 가능한 시드를 사용해야 하는 보다 복잡한 분포나 난수 시퀀스가 필요한 시나리오의 경우, 기본 제공 기능을 넘어서는 추가 라이브러리를 조사해야 할 수 있습니다.

## 심층 분석

Clojure를 포함한 대부분의 프로그래밍 언어에서 난수를 생성하는 기본 메커니즘은 일반적으로 의사 난수 생성기(PRNG)에 의존합니다. PRNG는 난수의 속성을 근사하는 숫자 시퀀스를 생성하는 알고리즘을 사용합니다. 알고리즘적으로 생성되기 때문에 이들은 진정으로 무작위적이지는 않지만, 대부분의 실용적인 목적에는 충분할 수 있다는 점을 지적하는 것이 가치가 있습니다.

컴퓨팅 초기에 고품질의 난수를 생성하는 것은 상당한 도전이었으며, 무작위성과 분포를 개선하기 위한 다양한 알고리즘 개발로 이어졌습니다. `rand` 및 `rand-int`와 같은 Clojure의 내장 함수는 일상적인 사용에 편리하며, 일반적인 사용 사례의 광범위한 스펙트럼을 다룹니다.

그러나 암호화 보안이 필요하거나 더 복잡한 통계적 샘플링 방법이 필요한 응용 프로그램의 경우, Clojure 개발자들은 보다 강력하고 전문화된 PRNG를 제공하는 외부 라이브러리를 자주 사용합니다. `clj-random`과 같은 라이브러리는 알고리즘의 더 넓은 다양성과 시드 제어에 대한 더 큰 제어력을 제공하며, 시뮬레이션, 암호화 응용 프로그램 또는 난수 시퀀스의 품질 및 예측 가능성이 중요한 의미를 가질 수 있는 모든 영역에 중요할 수 있습니다.

Clojure의 내장 난수 생성 기능이 많은 작업에 충분할 수 있지만, 외부 라이브러리를 탐색하는 것은 맞춤형 또는 더 중요한 응용 프로그램에 대한 더 깊은 통찰력과 옵션을 제공할 수 있습니다.
