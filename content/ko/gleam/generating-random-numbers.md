---
title:                "임의의 숫자 생성"
html_title:           "Gleam: 임의의 숫자 생성"
simple_title:         "임의의 숫자 생성"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

[Gleam](https://gleam.run/)은 함수형 프로그래밍 언어로, 모든 것이 함수로 이루어져 있습니다. 그 중 한 가지 중요한 기능이 있는데, 그것은 바로 난수 생성입니다. 여러분은 아마 난수를 여러분의 프로그램에서 사용해 본적이 있을 것입니다. 하지만 그것이 정확히 무엇인지 알고 있나요? 

## 무엇 & 왜?
난수 생성이란 무엇일까요? 난수 생성은 컴퓨터가 임의로 생성하는 숫자를 말합니다. 이는 보통 컴퓨터를 이용해서 다양한 기능을 수행하는 프로그램에서 사용됩니다. 이는 예측할 수 없는 동작을 만들어내기 위해 프로그래머들이 자주 사용하는 기능 중 하나입니다.

## 사용 방법:
다음은 Gleam에서 난수를 생성하는 방법입니다. 

```
Gleam.Random.int(1, 10)
```

위 코드는 1부터 10까지의 숫자 중에서 임의의 정수를 생성합니다. 또한 다른 범위의 숫자를 생성하고 싶다면 다음과 같이 적용할 수 있습니다.

```
Gleam.Random.int(100, 200)
```

이와 같은 방식으로 여러분은 원하는 범위의 난수를 생성할 수 있습니다.

## 깊이 파헤치기:
난수 생성에 대해 들어본 적이 없는 분들을 위해 간단하게 설명하자면, 컴퓨터는 사실 난수를 생성할 수 없습니다. 그래서 우리는 의사 난수 생성기(Pseudorandom Number Generator)를 사용합니다. 이는 시드(seed)라고 불리는 값에 의존하여 난수를 생성하는 알고리즘입니다. 이것이 바로 오늘날의 난수 생성 방식이며, 다양한 구현 방법이 존재합니다. 하지만 Gleam에서는 이를 위해 Mersenne Twister 알고리즘을 사용합니다. 이 알고리즘은 1997년에 개발되어 널리 사용되고 있습니다.

## 관련 자료:
난수 생성과 관련된 더 많은 정보를 얻고 싶다면 아래 링크들을 참고하세요.

- [Gleam 공식 문서](https://gleam.run/book/core_api.html#rand-module)
- [의사 난수 생성기(Pseudorandom Number Generator)에 관한 더 자세한 설명](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [다른 언어의 난수 생성 기능 비교](https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(random_number_generation))

크게 생각하지 말고, 시작해 보세요! 다양한 프로그램에서 난수 생성을 사용하다 보면 그 중요성을 더욱 깊이 이해하게 될 것입니다.