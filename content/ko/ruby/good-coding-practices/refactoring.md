---
date: 2024-01-26 03:37:40.488331-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\
  \uB4DC\uB97C \uC7AC\uAD6C\uC870\uD654\uD558\uB294 \uACFC\uC815\uC73C\uB85C\uC11C\
  , \uC678\uBD80 \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758\
  \ \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131, \uC608\uB97C \uB4E4\uC5B4 \uAC00\uB3C5\uC131\
  , \uBCF5\uC7A1\uC131 \uAC10\uC18C, \uC720\uC9C0\uBCF4\uC218 \uAC1C\uC120 \uB610\uB294\
  \ \uC131\uB2A5 \uD5A5\uC0C1 \uB4F1\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uB9AC\
  \uD329\uD1A0\uB9C1\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.007814-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\uB4DC\
  \uB97C \uC7AC\uAD6C\uC870\uD654\uD558\uB294 \uACFC\uC815\uC73C\uB85C\uC11C, \uC678\
  \uBD80 \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 방법:
제곱합을 계산하는 Ruby 메소드를 리팩토링하는 예를 통해 살펴보겠습니다.

**리팩토링 전:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # 출력: 14
```

**리팩토링 후:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # 출력: 14
```

리팩토링된 버전은 Ruby Enumerables를 사용하여 동일한 로직을 더 간결하고 명확하게 표현합니다. `map` 메소드는 각 요소를 변환하고, `sum`은 그 값들을 집계하여, 수동 루프 관리와 변수 할당의 필요성을 없앱니다.

## 깊게 들여다보기
리팩토링은 소프트웨어 개발 초기 관행으로 거슬러 올라가는 풍부한 역사적 맥락을 가지고 있습니다. 최초 언급은 1990년대로 거슬러 올라가며, Martin Fowler이 "리팩토링: 기존 코드의 디자인 개선"이라는 책에서 리팩토링을 위한 패턴 카탈로그를 제공하며 중요한 기여를 했습니다. 이후 리팩토링은 애자일 개발 관행의 핵심 요소가 되었습니다.

리팩토링에 대한 대안을 논할 때는 '다시 쓰기', 즉 오래된 시스템을 부분적으로 혹은 완전히 대체하거나, '코드 리뷰'와 '페어 프로그래밍'과 같은 관행을 적응하여 코드 품질을 점진적으로 개선하는 다른 접근 방법을 고려해야 합니다. 그러나 이것들은 리팩토링의 대체제가 아니라 과정을 보완합니다.

구현 측면에서, Ruby는 리팩토링 후에 종종 더 짧고 읽기 쉬운 코드를 결과로 낳는 훌륭하고 표현력이 풍부한 문법을 제공합니다. 핵심 원칙에는 DRY(반복하지 않기), 의미 있는 이름 사용하기, 메소드를 짧고 하나의 작업에 집중하기, 위의 예에서 볼 수 있듯이 Ruby의 Enumerable 모듈을 효과적으로 사용하기 등이 포함됩니다. RuboCop과 같은 자동화 도구는 프로그래머가 리팩토링에서 이득을 볼 수 있는 코드의 부분을 식별하는 데 도움을 줄 수도 있습니다.

## 참고 자료
Ruby에서의 리팩토링을 더 깊이 파고들고 싶다면 다음 자료들을 확인하세요:

- Martin Fowler의 기념비적인 책: [리팩토링: 기존 코드의 디자인 개선](https://martinfowler.com/books/refactoring.html)
- 더 깨끗한 코드 작성을 위한 Ruby 스타일 가이드: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, 정적 코드 분석기(린터) 및 포매터: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
