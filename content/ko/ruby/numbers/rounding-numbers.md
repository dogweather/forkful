---
date: 2024-01-26 03:46:49.855331-07:00
description: "\uBC29\uBC95: \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uAC83\
  \uC740 \uC0C8\uB85C\uC6B4 \uAC83\uC774 \uC544\uB2C8\uBA70, \uC778\uAC04\uC740 \uC218\
  \uC138\uAE30 \uB3D9\uC548 \uACC4\uC0B0\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\uAC70\
  \uB098 \uADF8\uB4E4\uC758 \uB3C4\uAD6C\uC758 \uD55C\uACC4 \uB0B4\uC5D0\uC11C \uC791\
  \uC5C5\uD558\uAE30 \uC704\uD574 \uBC18\uC62C\uB9BC\uD574 \uC654\uC2B5\uB2C8\uB2E4\
  . Ruby\uC5D0\uC11C `round` \uBA54\uC18C\uB4DC\uB294 \uAE30\uBCF8\uC801\uC73C\uB85C\
  \ \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC815\uC218\uB85C \uBC18\uC62C\uB9BC\uD558\uAC70\
  \uB098 \uC9C0\uC815\uB41C \uC18C\uC218\uC810 \uC790\uB9AC\uAE4C\uC9C0 \uBC18\uC62C\
  \uB9BC\uD558\uB294 \uB2A5\uB825\uC744\u2026"
lastmod: '2024-04-05T21:53:57.549070-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uAC83\uC740 \uC0C8\uB85C\
  \uC6B4 \uAC83\uC774 \uC544\uB2C8\uBA70, \uC778\uAC04\uC740 \uC218\uC138\uAE30 \uB3D9\
  \uC548 \uACC4\uC0B0\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\uAC70\uB098 \uADF8\uB4E4\
  \uC758 \uB3C4\uAD6C\uC758 \uD55C\uACC4 \uB0B4\uC5D0\uC11C \uC791\uC5C5\uD558\uAE30\
  \ \uC704\uD574 \uBC18\uC62C\uB9BC\uD574 \uC654\uC2B5\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
```Ruby
# 기본 반올림
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# 정밀도 지정
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# 내림
puts 2.9.floor          # => 2

# 올림
puts 2.1.ceil           # => 3

# 0 쪽으로 반올림
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

샘플 출력:
```
3
3
3.14
2.68
2
3
-3
-2
```

## 심층 탐구
숫자를 반올림하는 것은 새로운 것이 아니며, 인간은 수세기 동안 계산을 용이하게 하거나 그들의 도구의 한계 내에서 작업하기 위해 반올림해 왔습니다. Ruby에서 `round` 메소드는 기본적으로 가장 가까운 정수로 반올림하거나 지정된 소수점 자리까지 반올림하는 능력을 가지고 있어 다재다능합니다.

`round`의 대안으로 항상 내림하는 `floor`와 숫자의 값에 관계없이 항상 올림하는 `ceil`이 있습니다. 소수점을 그냥 잘라내려면 `truncate`가 있습니다.

역사적으로 볼 때 컴퓨터와 관련하여 부동소수점 연산을 다룰 때 내재된 부정확함 때문에 반올림이 중요해집니다. Ruby는 대부분의 언어와 마찬가지로 부동소수점 수에 대한 IEEE 754 표준을 따릅니다. 이는 프로그래머들이 예측하고 의존할 수 있는 방식으로 반올림을 처리한다는 것을 의미합니다.

그러나 더 많은 것이 있습니다. 은행업자 반올림(또한 반 약수에서 짝수로 반올림으로 알려짐)과 같은 개념은 `round` 메소드가 기본 제공하지 않기 때문에 Ruby 개발자가 수동으로 구현해야 할 수도 있습니다.

## 참고
- [Ruby Documentation](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round)의 Floats의 `round` 메소드.
- [부동소수점 산술에 대한 IEEE 표준 (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- 컴퓨터가 소수점 숫자를 어떻게 처리하는지에 대한 더 깊은 통찰을 위한 [부동소수점 정밀도 이해하기](https://floating-point-gui.de/).
