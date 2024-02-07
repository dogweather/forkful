---
title:                "복소수 다루기"
date:                  2024-01-26T04:45:52.260669-07:00
model:                 gpt-4-0125-preview
simple_title:         "복소수 다루기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
복소수는 실수부와 허수부(예: 3+4i)로 구성되며, 엔지니어링과 물리학에서 주요하게 사용됩니다. 프로그래머들은 시뮬레이션, 신호 처리, 실수만으로는 해결되지 않는 방정식을 푸는 데 이를 활용합니다.

## 사용 방법:
Ruby는 복소수를 다루기 쉽게 만들어 줍니다. Complex 클래스를 사용하여 생성하고 조작할 수 있습니다:

```ruby
require 'complex'

# 복소수 생성
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# 기본 연산
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# 공액, 크기, 그리고 위상
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 라디안

# 복소수 전용 메소드
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## 심화 학습
복소수는 새로운 개념이 아니며, 16세기부터 실수 해결책이 없는 방정식을 푸는 데 사용되었습니다. 수학을 넘어서, 계산상으로 Ruby의 Complex 클래스가 트리고노메트리(삼각) 및 초월 함수를 위한 Math 모듈에 의해 지원되며 무거운 계산을 수행합니다.

이전 프로그래밍 언어는 실수부와 허수부를 수동으로 다루는 것을 요구했습니다. Fortran과 C++와 같은 일부 언어는 복소수 산술을 위한 특별한 라이브러리를 사용합니다.

Ruby의 접근 방식은 복소수 지원을 문법으로 내장시켜, 바퀴를 다시 발명할 필요가 없게 합니다. 내부적으로 Complex 클래스가 수학을 처리하면서 Ruby는 객체 상호작용을 책임집니다.

## 참고
- Ruby 문서에서의 Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorld에서 복소수에 대한 설명: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- 복소수와 그 유용성에 대한 시각적 소개: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
