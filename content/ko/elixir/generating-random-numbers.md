---
title:                "랜덤 숫자 생성하기"
html_title:           "Elixir: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

개요:
많은 프로그래밍 언어들은 난수 생성에 대한 내장 함수를 제공합니다. 그렇지만, 왜 객체지향적이고 동시성을 지원하는 Elixir로 난수를 생성해야 할까요? 이 글에서 난수 생성을 활용하는 이유와 Elixir로 어떻게 난수를 생성할 수 있는지, 그리고 깊이 들어가는 정보를 알아보겠습니다.

## 왜

난수 생성은 많은 소프트웨어에 있어서 필수적인 요소입니다. 무엇보다도 게임, 보안, 랜덤한 테스트 데이터 생성 등에 활용됩니다. Elixir는 객체지향적인 언어로서부터 받은 모듈, 함수, 코루틴 등의 개념을 활용해 난수 생성을 더욱 간편하고 유연하게 할 수 있습니다.

## How To

Elixir는 `:rand` 라이브러리를 통해 난수 생성을 제공해줍니다. `:rand`는 `integer`, `uniform`, `uniform_float` 등의 여러가지 함수를 제공하며 각각 다른 난수를 생성해줍니다.

아래는 `integer` 함수를 사용해 0과 100 사이의 랜덤한 정수를 출력하는 예제입니다.

```Elixir
:rand.integer(0..100)
```

출력 예시:

```Elixir
44
```

동일한 방식으로 `uniform` 함수를 사용해 0.0과 1.0 사이의 랜덤한 숫자를 출력할 수 있습니다.

```Elixir
:rand.uniform()
```

출력 예시:

```Elixir
0.6855220130271914
```

더욱 정교한 난수 생성을 위해서는 `uniform_float` 함수를 사용할 수 있습니다. 아래는 0.0과 100.0 사이의 소수 두 자리수를 가진 랜덤한 숫자를 출력하는 예제입니다.

```Elixir
:rand.uniform_float(0.0..100.0, 2)
```

출력 예시:

```Elixir
45.32
```

위에서 소개한 세 가지 함수 외에도 `:rand` 라이브러리에는 다양한 난수 생성 함수가 있으니 자세한 것은 Elixir 공식 문서를 참고하기를 추천합니다.

## 깊이 들어가기

위에서 소개한 함수들은 모두 시드값을 인자로 받을 수 있습니다. 이를 통해 더욱 다양한 형태의 난수를 생성할 수 있습니다. 예를 들어, 동일한 시드값을 사용하면 항상 같은 결과를 출력하게 됩니다.

```Elixir
:rand.uniform_float(0.0..100.0, 2, 1234)
```

출력 예시:

```Elixir
17.12
```

반면에 시드값을 제외하고 함수를 호출하면 시스템 시간을 기반으로 랜덤한 시드값을 생성해줍니다.

더욱 심화된 이야기로, Elixir에서 제공하는 `:random` 모듈을 활용해 난수 생성과 관련된 세부적인 제어도 가능합니다. 이에 대한 자세한 내용은 공식 문서를 참고해주시기 바랍니다.

## See Also

- Elixir 공식 문서: https://elixir-lang.org/docs.html
- Elixir의 비동기 처리 능력: https://blog.appscode.com/elixir-async-processing-b08070e138b0
- 함수형 프로그래밍과 Elixir: https://medium.com/@BastiHz/functional-programming-in-elixir-46c63e820fbd