---
title:                "랜덤 숫자 생성"
html_title:           "Elixir: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 숫자 생성은 랜덤하게 선택된 숫자를 생성하는 것입니다. 프로그래머들은 이를 사용하여 게임 시스템, 보안 기능, 테스트 등 다양한 분야에서 랜덤성을 제공할 수 있습니다.

## 방법:

```Elixir
# Elixir에서의 랜덤 숫자 생성 예제
# 1부터 10까지의 숫자 중 랜덤하게 선택하기
1..10 |> Enum.random()
# 출력 결과: 7

# 특정 범위 내에서 여러 숫자 랜덤하게 선택하기
Enum.random(1..100, 3)
# 출력 결과: [55, 23, 78]

# 표준 정규 분포를 따르는 랜덤한 실수 생성하기
:rand.normal()
# 출력 결과: 0.043783642256075855
```

## 심층 분석:

1. 랜덤 숫자 생성은 컴퓨터의 의사 난수 알고리즘에 의해 가능해졌으며 이전에는 난수 테이블을 사용하여 구현되었습니다.
2. Elixir에서는 `Enum.random/2` 함수를 사용하여 범위 내에서 랜덤 숫자를 선택할 수 있으며, `rand.normal/0` 함수를 사용하여 표준 정규 분포에 따른 랜덤 실수를 생성할 수 있습니다.
3. 기본적으로 Elixir의 난수 생성 함수는 seed 값을 지원하지 않습니다. 따라서 보안을 위해서는 Cryptography 모듈을 사용하여 seed 값을 설정하고 더 강력한 랜덤 숫자를 생성할 수 있습니다.

## 관련 자료:

- [Elixir 공식 문서 - 랜덤 숫자 생성](https://hexdocs.pm/elixir/Kernel.html#random/0)
- [Elixir 커뮤니티 포럼 - 랜덤 숫자 생성 관련 토론](https://elixirforum.com/t/how-to-generate-n-numbers-randomly-from-a-list/14525)