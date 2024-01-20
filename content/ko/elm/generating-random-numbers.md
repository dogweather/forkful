---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜?

랜덤 숫자를 생성하는 것은, 예측할 수 없는 수를 생성하는 것을 말합니다. 프로그래머들은 간혹 현실 세계의 불확실성을 시뮬레이션하거나, 테스트 데이터를 만들기 위해 이를 사용합니다.

## 어떻게 하는가:

아래는 Elm에서 Random 모듈을 사용하여 랜덤 숫자를 생성하는 예제입니다.

```Elm
import Random

main =
    Random.generate identity (Random.int 1 100)
```

이 코드는 1에서 100사이의 무작위 정수를 생성합니다.

## 깊게 파보기:

### 역사적 맥락

랜덤 숫자 생성은 컴퓨터 과학의 초창기부터 핵심 기능으로 자리 잡았습니다. 프로그램 중에서도 시뮬레이션, 암호화, 갬블링 등에서 주로 쓰였습니다.

### 대안

Elm 외에도 JavaScript, Python 등 다양한 언어에서 랜덤 숫자를 생성할 수 있는 기능이 있습니다. 각 언어는 다양한 방식으로 랜덤성을 제공합니다.

### 구현 세부 정보

Elm에서 랜덤 숫자를 생성하는 것은 순수 함수를 이용해야 합니다. 즉, 랜덤 숫자 생성 함수는 항상 동일한 입력에 대해서는 동일한 결과를 반환해야 합니다. Elm은 이 순수성을 유지하기 위해 Task나 Cmd와 같은 별도의 컨텍스트에서 랜덤 값을 생성합니다.

## 참고 자료:

- [Elm의 Random 모듈 문서](https://package.elm-lang.org/packages/elm/random/latest/)
- [다른 언어에서의 랜덤 숫자 생성 방법](https://www.geeksforgeeks.org/random-number-generator-in-arbitrary-probability-distribution-fashion/)