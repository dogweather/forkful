---
title:    "Elm: 표준 에러에 쓰는 것"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜

Elm 프로그래밍을 하면서 standard error를 작성하는 데 왜 관여하는지 알아보겠습니다.

## 어떻게

```Elm
module Main exposing (..)

import Debug exposing (log)

timesTen : Int -> Int
timesTen x =
  let
      result = x * 10
  in
      log ("결과: " ++ toString result)
      result

main =
  timesTen 5
```

위 코드는 5에 10을 곱한 후 결과를 로그로 남기는 간단한 예시입니다. 실행하면 다음과 같은 결과가 출력됩니다.

```
결과: 50
```

위의 예시처럼 `Debug.log` 함수를 사용하면, 우리가 원하는 값을 터미널에 로그로 남길 수 있습니다. 이를 통해 디버깅할 때 유용하게 사용할 수 있습니다.

## 깊이 파헤치기

standard error를 작성하는 데에는 여러가지 이유가 있습니다. 가장 보편적인 이유는 프로그래밍 중 발생할 수 있는 오류를 파악하고 수정하기 위해서 입니다. 터미널에 로그를 남겨서 어떤 값이나 변수가 어떤 식으로 작동하는지 확인할 수 있습니다. 또 다른 이유는 오류 메시지를 사용자들에게 노출하고, 애플리케이션의 안정성을 높이기 위해서 입니다.

## 또 보기

- [Elm 공식 문서 - Debug 모듈](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm에 대해 알아보기](https://ko.wikipedia.org/wiki/Elm_(%EC%96%BC%EC%83%89_%EC%96%B8%EC%96%B4))