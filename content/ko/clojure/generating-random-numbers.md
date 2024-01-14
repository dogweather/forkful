---
title:                "Clojure: 랜덤 숫자 생성"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

Korean:

## 왜
랜덤 숫자를 생성하는 것에 참여하는 이유는 무엇일까요? 일반적으로 우리는 사람이나 컴퓨터가 만든 패턴을 피하는 경향이 있습니다. 우리가 랜덤 숫자를 사용하는 이유는 이것이 우리에게 예측이 어려운 결과를 만들어내기 때문입니다. 이러한 예측 불가능성은 게임이나 암호화 등 다양한 영역에서 매우 중요합니다.

## 방법
랜덤 숫자를 생성하는 가장 간단한 방법은 `rand` 함수를 사용하는 것입니다. 이 함수는 0과 1 사이의 실수를 랜덤하게 생성해줍니다. 예를 들면:

```Clojure
(rand) ; => 0.7187084376468706
```

만약 우리가 특정 범위의 숫자를 원한다면 `rand-int` 함수를 사용하면 됩니다. 예를 들면, 1부터 10 사이의 숫자를 생성하기 위해서는 다음과 같이 할 수 있습니다.

```Clojure
(rand-int 10) ; => 6
```

## 딥 다이브
랜덤 숫자를 생성하는 원리에 대해 깊이 알아보고 싶다면, 우리는 유사 난수 생성기를 살펴볼 필요가 있습니다. 랜덤 숫자 생성에서 가장 중요한 개념은 재현 가능성입니다. 유사 난수 생성기는 어떤 시드(seed) 값을 사용하여 숫자를 생성하는데, 이 시드 값을 바꾸면 다른 숫자가 나오게 됩니다. 만약 시드 값을 고정하면 항상 동일한 결과를 얻을 수 있습니다.

## 참고
- [ClojureDocs - rand](https://clojuredocs.org/clojure.core/rand)
- [ClojureDocs - rand-int](https://clojuredocs.org/clojure.core/rand-int)
- [Wikipedia - Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)