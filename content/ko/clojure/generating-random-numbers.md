---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?
랜덤 번호 생성은 예측 불가능한 번호 덩어리를 만들어내는 것을 의미합니다. 이는 보안, 게임, 실험 등에서 중요한 역할을 합니다.

## 어떻게 하는가:
랜덤 숫자를 생성하려면, 아래의 `rand` 또는 `rand-int` 함수를 사용하면 됩니다.

```clojure
(rand) 
```

위의 함수는 0.0과 1.0 사이의 랜덤한 실수를 반환합니다. 

```clojure
(rand-int 100)
```
`rand-int` 함수는 주어진 범위 내에서 랜덤 정수를 생성합니다. 위 코드는 0에서 99까지의 랜덤 정수를 생성합니다. 

## 깊게 알아보기
랜덤 번호 생성은 20세기 초부터 컴퓨터 시스템에 크게 활용되기 시작했습니다. 랜덤성은 아래와 같은 상황에서 크게 중요성을 갖습니다:

1. 보안: 비밀번호, 암호키 등을 생성할 때
2. 시뮬레이션: 예상 결과를 테스트하는데 사용
3. 게임: 요소의 무작위성을 보장

Clojure에서 공식적으로 제공하는 `rand` 함수 외에도, 다양한 라이브러리에서 더 많은 기능을 제공합니다.

## 참고 자료
- Clojure 공식 문서의 [랜덤 함수에 대한 섹션](https://clojure.org/guides/learn/functions#_random_numbers)
- Alan Tate의 ["Clojure에 대하여"](https://martinfowler.com/articles/clojure-antilibrary.html), Martin Fowler의 블로그 (영문)
- [선형 합동 방법](https://en.wikipedia.org/wiki/Linear_congruential_generator), 랜덤 수의 전통적인 생성 방법에 대한 위키피디아 페이지.