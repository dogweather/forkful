---
date: 2024-01-27 20:35:20.440436-07:00
description: "\uBC29\uBC95: \uB8E8\uBE44\uB294 \uC8FC\uB85C `Random` \uD074\uB798\uC2A4\
  \uB97C \uD1B5\uD574 \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\
  \uD55C \uC5EC\uB7EC \uBA54\uC11C\uB4DC\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uAE30\
  \uBCF8 \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD558\uAE30."
lastmod: '2024-03-13T22:44:55.985877-06:00'
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uB294 \uC8FC\uB85C `Random` \uD074\uB798\uC2A4\uB97C \uD1B5\
  \uD574 \uBB34\uC791\uC704 \uC218\uB97C \uC0DD\uC131\uD558\uAE30 \uC704\uD55C \uC5EC\
  \uB7EC \uBA54\uC11C\uB4DC\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

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
