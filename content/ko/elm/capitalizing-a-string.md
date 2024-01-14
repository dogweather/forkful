---
title:    "Elm: 문자열 대문자로 변환하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열을 대문자로 변환하는 것에 대해 고민해 본 적이 있나요? 당신이 알기 원하는 것은 어떤 이유로 이 작업이 유용한지에 대한 것입니다. 그래서 오늘은 Elm 프로그래밍에서 문자열을 대문자로 변환하는 방법에 대해 알아보겠습니다.

# 어떻게

이 작업을 수행하는 가장 쉬운 방법은 Elm의 String 라이브러리에 있는 `toUpper` 함수를 사용하는 것입니다.

```Elm 
import String

String.toUpper "hello" -- "HELLO"
```

`toUpper` 함수는 문자열을 모두 대문자로 변환하여 반환합니다. 위 예제에서는 "hello"라는 문자열이 대문자로 변환되어 "HELLO"가 출력됩니다.

# 깊게 파헤치기

이 예제에서 사용한 `toUpper` 함수는 사실 `toUpper : String -> String`라는 타입 시그니처를 가지고 있습니다. 이는 `toUpper` 함수가 단일 문자열을 인수로 받아 대문자로 변환된 문자열을 반환한다는 것을 의미합니다.

또한, 이 함수는 인수로 받은 문자열 외에도 `foldl`이라는 또 다른 함수를 이용해 `toUpper`를 적용할 수 있는 리스트도 처리할 수 있습니다. 이를 통해 여러 문자열을 대문자로 변환하는 것도 가능합니다.

# 더 알아보기

이외에도 `String.split` 함수를 사용해 문자열을 분할하여 대문자로 변경하는 등 다양한 방법으로 문자열을 대문자로 변환할 수 있습니다. Elm의 String 라이브러리에는 문자열을 다루는 다양한 함수들이 많이 있기 때문에 많은 공부가 필요할 수도 있습니다. 하지만 한 번 익숙해지고 나면 문자열을 다루는 작업에서 매우 유용하게 사용할 수 있습니다.

# 이어서 읽어보세요

* [Elm 공식문서 - String 라이브러리](https://package.elm-lang.org/packages/elm/core/latest/String)
* [Elm 공식문서 - 함수 연산자](https://guide.elm-lang.org/appendix/functions.html)
* [코드스쿼드 블로그 - Elm 문자열 다루기](https://medium.com/@codesquad_yoda/elm-문자열-다루기-13545fd90dea)