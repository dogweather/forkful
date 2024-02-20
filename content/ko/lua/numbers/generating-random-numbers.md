---
date: 2024-01-27 20:34:49.221764-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\
  \uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uAC8C\uC784, \uBCF4\uC548 \uC751\uC6A9 \uD504\
  \uB85C\uADF8\uB7A8 \uB4F1 \uB2E4\uC591\uD55C \uBAA9\uC801\uC73C\uB85C \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uB294 \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC22B\uC790 \uAC12\
  \uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uD504\uB85C\uC81D\uD2B8\uC5D0 \uBD88\uD655\uC2E4\uC131 \uC694\
  \uC18C\uB97C \uB3C4\uC785\uD558\uAC70\uB098 \uC2E4\uC0DD\uD65C \uBCC0\uB3D9\uC131\
  \uC744 \uBAA8\uBC29\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.323753
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740\
  \ \uC2DC\uBBAC\uB808\uC774\uC158, \uAC8C\uC784, \uBCF4\uC548 \uC751\uC6A9 \uD504\
  \uB85C\uADF8\uB7A8 \uB4F1 \uB2E4\uC591\uD55C \uBAA9\uC801\uC73C\uB85C \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uB294 \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC22B\uC790 \uAC12\
  \uC744 \uC0DD\uC131\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uD504\uB85C\uC81D\uD2B8\uC5D0 \uBD88\uD655\uC2E4\uC131 \uC694\
  \uC18C\uB97C \uB3C4\uC785\uD558\uAC70\uB098 \uC2E4\uC0DD\uD65C \uBCC0\uB3D9\uC131\
  \uC744 \uBAA8\uBC29\uD558\uAE30 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
---

{{< edit_this_page >}}

## 무엇 & 왜?

프로그래밍에서 난수 생성은 시뮬레이션, 게임, 보안 응용 프로그램 등 다양한 목적으로 사용할 수 있는 예측할 수 없는 숫자 값을 생성하는 것입니다. 프로그래머들은 프로젝트에 불확실성 요소를 도입하거나 실생활 변동성을 모방하기 위해 이 기능을 사용합니다.

## 방법:

Lua는 `math.random` 함수를 통해 난수를 생성하기 위한 내장 지원을 제공합니다. 이 함수는 원하는 출력에 따라 여러 가지 방법으로 사용할 수 있습니다:

1. **0과 1 사이의 무작위 부동소수점 숫자 생성:**

```Lua
print(math.random())
```

샘플 출력은 `0.13117647051304`일 수 있습니다. 각 실행은 다른 값을 생성합니다.

2. **지정된 범위 내에서 무작위 정수 생성:**

두 경계 내에서 무작위 정수를 생성하려면 먼저 `math.randomseed(os.time())`를 사용하여 시드를 설정하여 변동성을 주고, 그 다음 `math.random`을 두 인자와 함께 호출합니다:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) --1과 10 사이의 무작위 정수를 생성합니다
```

샘플 출력은 `7`일 수 있습니다. 이 경우에도 실행할 때마다 출력은 달라집니다.

`math.randomseed`로 시드를 설정하는 것이 중요한데, 그것 없이는 `math.random`이 프로그램이 실행될 때마다 동일한 숫자 시퀀스를 생성할 수 있기 때문입니다. 일반적으로, 현재 시간인 `os.time()`으로 시딩하면 실행마다 다른 시퀀스를 보장합니다.

## 심층 분석

Lua(그리고 대부분의 프로그래밍 언어)에서 난수를 생성하는 메커니즘은 실제로 무작위가 아니라 의사랜덤으로, 알고리즘에 의해 생성됩니다. 이러한 의사랜덤 숫자 생성기(PRNGs)는 결정적이며, 숫자 생성 시퀀스를 시작하기 위해 시드 값을 요구합니다. 시드 선택은 난수의 질에 있어 매우 중요한데, 이것이 현재 시간을 사용하는 것이 일반적인 관행인 이유입니다.

역사적으로, Lua의 난수 생성 기능은 발전해왔습니다. 초기 버전은 구현에 따라 품질과 성능이 다른 C 표준 라이브러리의 `rand()` 함수에 의존했습니다. 현재 버전의 Lua는 기본 플랫폼에 따라 더 강력한 메커니즘을 사용할 수 있게 함으로써, 난수 생성에서 더 큰 일관성과 유용성을 제공합니다.

암호학 수준의 난수가 필요한 프로젝트의 경우, PRNG의 결정적 특성 때문에 내장 Lua 기능만으로는 충분하지 않을 수 있습니다. 이러한 경우, 프로그래머들은 고보안 응용 프로그램에 적합한 비결정적 난수를 제공할 수 있는 외부 라이브러리나 시스템 특정 API로 전환하는 경우가 많습니다.
