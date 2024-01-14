---
title:    "Elm: 랜덤 숫자 생성하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

우리는 일상 생활에서 무작위로 발생하는 다양한 사건을 경험합니다. 그리고 때로는 이러한 사건을 모방하는 컴퓨터 프로그램을 만들기도 합니다. 이때 사용되는 함수 중 하나가 "random" 함수인데, 이 함수를 이용하면 우리는 쉽게 무작위로 숫자를 생성할 수 있습니다. 즉, 우리가 만든 프로그램을 더 다양하게 만들고 싶을 때, 무작위 숫자를 생성할 필요가 생기는 것입니다.

## 사용 방법

Elm에서는 "Random" 모듈을 통해 무작위 숫자를 생성할 수 있습니다. 먼저 모듈을 import 하고, 생성하고자 하는 숫자의 범위를 지정해야 합니다. 아래는 1에서 100 사이의 무작위 숫자를 생성하는 간단한 예제 코드입니다.

```
import Random
Random.generate (Random.int 1 100) identity
```

실행 결과는 아래와 같이 나타납니다.

```
42
```

여기서 "identity"는 결과 값에 변화를 주지 않고 그대로 출력하도록 하는 함수입니다. 이 밖에도 다양한 함수를 사용할 수 있으며, 나눔 기호를 생성하려면 "Random.int 1 10" 대신 "Random.int 0 9"로 변경해주면 됩니다.

## 더 깊게 들어가기

Elm에서 "Random" 모듈을 사용하면 무작위로 숫자만 생성하는 것뿐만 아니라, 무작위로 순서를 섞거나 열거형(enum) 값을 생성할 수도 있습니다. 이 모듈을 사용하면서 가장 중요한 것은 "Seed"라는 항목입니다. "Seed"는 무작위 숫자를 생성할 때 사용되는 초기값으로, 이 값을 변경해주면 무작위성을 조절할 수 있습니다. 예를 들어 같은 "Seed" 값을 사용하면 항상 같은 결과를 얻을 수 있습니다.

만약 "Seed"를 임의로 변경해준 뒤에 원래 값으로 되돌리고 싶다면, "Random.initialSeed" 함수를 이용하면 됩니다. 또한 "Seed"를 변경할 때마다, 그에 따라 생성되는 숫자의 범위 및 출력값도 달라지므로 주의가 필요합니다.

## 더 많은 정보 알아보기

더 자세한 정보는 아래 링크들을 참고해보세요.

- [Elm 공식 문서](https://guide.elm-lang.org/effects/random.html)
- [Elm 기반 무작위 색상 생성기 예제](https://github.com/seconde/elm-colormaker/)
- [Elm Community Forum](https://discourse.elm-lang.org/t/what-is-the-best-way-to-generate-random-numbers-in-elm/233)
- [Elm 블로그](https://elm-lang.org/news/how-to-deal-with-hidden-complexity)

## 더 알아보기

[엘름공간](https://elm.space)