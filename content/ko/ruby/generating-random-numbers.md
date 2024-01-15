---
title:                "랜덤 숫자 생성하기"
html_title:           "Ruby: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜

랜덤 숫자를 생성하는 것이 중요한 이유는 다양한 도메인에서 유용한 정보와 예측을 얻을 수 있기 때문입니다. 예를 들어 신용카드 번호 생성, 통계 분석, 시뮬레이션 등의 다양한 분야에서 랜덤 숫자를 활용할 수 있습니다.

## 방법

```Ruby
# 1부터 10 사이의 랜덤 숫자 출력하기
puts rand(1..10)
# => 6
```
```Ruby
# 배열 내에서 랜덤한 요소 출력하기
array = ["사과", "바나나", "딸기", "포도"]
puts array.sample
# => 바나나
```
```Ruby
# 기존의 랜덤 숫자 시퀀스를 다시 불러오기
srand 25
puts rand(1..10)
# => 5
srand 25
puts rand(1..10)
# => 5 (동일한 결과 출력)
```

## 심층 탐구

랜덤 숫자 생성은 알고리즘에 따라 다르게 동작합니다. Ruby에서는 Mersenne Twister 알고리즘을 사용하여 랜덤 숫자를 생성합니다. 이 알고리즘은 일관된 성능과 난수의 높은 품질을 보장합니다.

## 관련 링크

- [Ruby 공식 문서 - 랜덤 숫자 생성](https://ruby-doc.org/core-3.0.1/Random.html)
- [Mersenne Twister 알고리즘](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [랜덤 숫자 예제 코드](https://www.rubyguides.com/2018/10/ruby-random/)