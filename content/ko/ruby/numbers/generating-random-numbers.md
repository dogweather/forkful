---
date: 2024-01-27 20:35:20.440436-07:00
description: "\uB8E8\uBE44\uC5D0\uC11C \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\
  \uD558\uB294 \uAC83\uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uC554\uD638\uD654 \uBC0F\
  \ \uAC8C\uC784\uACFC \uAC19\uC740 \uC2DC\uB098\uB9AC\uC624\uC5D0\uC11C \uD544\uC218\
  \uC801\uC778, \uB17C\uB9AC\uC801\uC73C\uB85C \uC608\uCE21\uD560 \uC218 \uC5C6\uB294\
  \ \uC218\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC608\uCE21 \uBD88\uAC00\uB2A5\uC131\
  \uC744 \uCD94\uAC00\uD558\uAC70\uB098 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\
  \ \uD604\uC2E4 \uC138\uACC4\uC758 \uBCC0\uB3D9\uC131\uC744 \uBAA8\uBC29\uD558\uAE30\
  \ \uC704\uD574 \uBB34\uC791\uC704\uC131\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.898818
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uC5D0\uC11C \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD558\
  \uB294 \uAC83\uC740 \uC2DC\uBBAC\uB808\uC774\uC158, \uC554\uD638\uD654 \uBC0F \uAC8C\
  \uC784\uACFC \uAC19\uC740 \uC2DC\uB098\uB9AC\uC624\uC5D0\uC11C \uD544\uC218\uC801\
  \uC778, \uB17C\uB9AC\uC801\uC73C\uB85C \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC218\
  \uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC608\uCE21 \uBD88\uAC00\uB2A5\uC131\uC744\
  \ \uCD94\uAC00\uD558\uAC70\uB098 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0 \uD604\
  \uC2E4 \uC138\uACC4\uC758 \uBCC0\uB3D9\uC131\uC744 \uBAA8\uBC29\uD558\uAE30 \uC704\
  \uD574 \uBB34\uC791\uC704\uC131\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?

루비에서 무작위 수를 생성하는 것은 시뮬레이션, 암호화 및 게임과 같은 시나리오에서 필수적인, 논리적으로 예측할 수 없는 수를 생성하는 것을 포함합니다. 프로그래머들은 예측 불가능성을 추가하거나 애플리케이션에 현실 세계의 변동성을 모방하기 위해 무작위성을 사용합니다.

## 방법:

루비는 주로 `Random` 클래스를 통해 무작위 수를 생성하기 위한 여러 메서드를 제공합니다.

### 기본 무작위 수

기본 무작위 수를 생성하기:

```Ruby
puts rand(10) # 0과 9 사이의 무작위 수를 생성
```

### 범위 내에서의 무작위 수

특정 범위 내의 무작위 수를 위해:

```Ruby
puts rand(1..10) # 1과 10 사이의 무작위 수를 생성
```

### Random 클래스 사용하기

반복 가능한 무작위 수 순서를 생성하기 위해, 시드와 함께 `Random` 클래스를 사용할 수 있습니다.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # 예측 가능한 "무작위" 수를 생성
```

### 무작위 배열 요소 생성하기

배열에서 무작위 요소를 선택하기:

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # 배열에서 무작위로 요소를 선택
```

### 샘플 출력:

위의 각 코드 스니펫을 실행하면 그 무작위성 때문에 다른 출력을 생성할 것입니다. 예를 들어, `rand(10)`은 `7`을 출력할 수 있고, `colors.sample`은 `"green"`을 출력할 수 있습니다.

## 심층 탐구

컴퓨터 과학에서 무작위 수 생성의 개념은 컴퓨터가 결정론적 지시를 따르기 때문에 역설적입니다. 초기 방법은 예측 불가능성을 달성하기 위해 외부 입력에 크게 의존했습니다. Ruby의 무작위성은 광범위한 주기와 균일한 분포로 알려져 있어, 고품질의 무작위성을 요구하는 애플리케이션에 매우 적합한 메르센 트위스터 알고리즘을 기반으로 합니다.

루비의 내장 메서드는 대부분의 필요를 잘 충족시키지만, 모든 암호화 목적에는 충분하지 않을 수 있으며, 의사 무작위 수의 예측 가능성은 취약점이 될 수 있습니다. 암호화 보안을 위해, 루비 개발자들은 `OpenSSL::Random`과 같은 라이브러리를 탐색할 수 있으며, 이는 민감한 애플리케이션에 대한 높은 예측 불가능성을 보장하는 암호화 보안 무작위 수를 생성하도록 설계되었습니다.
