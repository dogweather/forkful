---
title:                "Ruby: 랜덤 숫자 생성"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜?

우리는 여러분이 프로그래밍을 하면서 "랜덤한" 요소를 통해 만들어진 결과를 만나기를 원합니다. 이는 재미있는 게임을 만들거나, 무언가를 무작위로 선택하는 등의 용도로 많이 사용됩니다.

## 어떻게?

랜덤한 숫자를 생성하는 가장 간단한 방법은 Ruby의 내장 라이브러리인 `rand()` 메소드를 사용하는 것입니다. 다음은 `rand()` 메소드를 사용한 간단한 예제 코드입니다:

```Ruby
# 1에서 10 사이의 랜덤한 숫자 생성
puts rand(1..10)

# 0에서 100 사이의 랜덤한 실수 생성
puts rand(0.0..100.0)
```

실행 결과는 다음과 같을 수 있습니다:

```
7
65.234142643
```

위의 예제 코드에서처럼, `rand()` 메소드는 인자로 넘겨진 값을 바탕으로 랜덤한 숫자를 생성합니다.

## 깊게 들어가기

Ruby의 `rand()` 메소드는 내부적으로 사전에 정의된 시드(seed) 값에 의존합니다. 따라서 프로그램이 실행될 때마다, 시드 값은 별개의 방식으로 설정됩니다. 이는 결과적으로 매번 실행할 때마다 다른 랜덤한 숫자가 생성됨을 의미합니다.

또한, Ruby에서는 `srand()` 메소드를 사용하여 시드 값을 수동으로 설정할 수도 있습니다. 이는 게임 또는 시뮬레이션에서 일관된 결과를 얻을 때 유용합니다.

## 관련 자료

- [Ruby의 난수 생성 방법: `rand()` vs `srand()`](http://ruby-doc.org/core-2.6.3/Kernel.html#method-i-rand)
- [Ruby에서의 난수 생성에 대한 깊은 이해](https://blog.appsignal.com/2018/07/31/ruby-magic-wild-and-uniform-pseudo-random-numbers.html)

# 또한 볼만한 것들

- [Mersenne Twister 알고리즘을 이용한 난수 생성 방법](https://www.helloruby.com/algorithm/random-number-generation-using-mersenne-twister)