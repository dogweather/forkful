---
title:                "Ruby: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤한 숫자를 생성하는 것에 대해 관심이 있을 수 있습니다. 이는 범위 내에서 무작위로 숫자를 생성할 수 있는 능력을 가지기 때문입니다. 이는 게임 및 시뮬레이션 등의 분야에서 유용하게 사용될 수 있습니다.

## 어떻게?

우선 Ruby의 기본적인 랜덤 라이브러리인 `rand` 메소드를 사용해 봅시다.

```Ruby
rand #=> 0.08419006418511081
```

이 메소드는 0 이상 1 미만의 랜덤한 소수를 생성합니다. 이제 이를 이용해 다음과 같은 코드를 작성해 봅시다.

```Ruby
rand(10) #=> 4
```

위 코드는 0 이상 10 미만의 랜덤한 정수를 생성합니다. 더 흥미로운 예제로는 다음과 같은 코드가 있습니다.

```Ruby
rand(1..10) #=> 7
```

위 코드는 1 이상 10 이하의 랜덤한 정수를 생성합니다. `..` 연산자를 사용하면 범위 내의 모든 값들이 가능한 범위가 됩니다. 마지막으로, 랜덤한 부동소수점을 생성하는 방법도 살펴봅시다.

```Ruby
rand(1.5..3.5) #=> 2.2871993837935454
```

이 방식으로 원하는 범위 내의 랜덤한 숫자를 생성할 수 있습니다.

## 깊이 파고들기

Ruby의 `rand` 메소드는 내부적으로 의사 난수 발생기(pseudo-random number generator)를 사용합니다. 이 의사 난수 발생기는 이전에 생성된 숫자를 이용해 다음 숫자를 예측하는 방식으로 랜덤한 수열을 생성합니다. 이러한 이유로 `rand` 메소드는 프로그램을 실행할 때마다 같은 순서의 랜덤한 숫자들을 생성합니다. 따라서 보안이 중요한 애플리케이션에서는 보다 안전한 알고리즘이 필요할 수 있습니다.

더 나아가서, Ruby의 `SecureRandom` 라이브러리는 보안 애플리케이션에서 랜덤한 값들을 생성하기 위한 다양한 방식을 제공합니다. 이를 이용해 더 안전한 랜덤한 숫자들을 생성할 수 있습니다.

## 관련 링크

- `rand` 메소드의 공식 문서: https://ruby-doc.org/core-3.0.0/Random.html#method-i-rand
- `SecureRandom` 라이브러리의 공식 문서: https://ruby-doc.org/stdlib-3.0.0/libdoc/securerandom/rdoc/SecureRandom.html