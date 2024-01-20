---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 수 생성은 예측할 수 없는 수를 만드는 것입니다. 프로그래머들은 실험, 시뮬레이션, 게임 등 다양한 상황에서 랜덤성이 필요하기 때문에 이를 사용합니다.

## 사용 방법:

Ruby에서 랜덤 수를 생성하는 방법을 보여드리겠습니다. 

```Ruby
rand_num = rand(100)
puts rand_num
```

위 코드는 0-99 사이의 랜덤 정수를 생성하고 출력합니다.

## 깊게 들어가보기

랜덤 수 생성은 오래전부터 과학, 엔지니어링, 수학에서 중요한 역할을 해 왔습니다. Ruby에서는 `rand` 메소드로 랜덤성을 구현했습니다. 이 방법 외에도 다양한 알고리즘으로 랜덤 수를 생성할 수 있습니다. 예를 들어, 메르센 트위스터 알고리즘이 있습니다.

랜덤수 생성의 핵심은 보안입니다. 의사 랜덤 수는 암호학에서 매우 중요하기 때문에, 이들을 예측할 수 없어야 합니다. Ruby의 `SecureRandom` 라이브러리는 보안 상황에서 랜덤 수를 생성하는데 사용됩니다.

```Ruby
require 'securerandom'

secure_rand_num = SecureRandom.random_number
puts secure_rand_num
```

## 함께 보기

3. [Wikipedia - Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)

이 글을 통해 Ruby에서 랜덤 수를 어떻게 생성하는지, 그리고 왜 그것이 중요한지 배웠기를 바랍니다. 더 깊게 파보고 싶으시다면 위의 링크를 참조하세요.