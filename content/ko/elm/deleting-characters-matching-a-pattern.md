---
title:    "Elm: 패턴과 일치하는 문자 삭제하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜?

본 글에서는 어떤 상황에서 문자열에서 일치하는 패턴의 문자를 삭제하는 작업을 해야하는지에 대해 알아보겠습니다.

## 하는 방법

먼저, Elm 언어에서는 문자열을 내장 함수를 사용하여 쉽게 다룰 수 있습니다. 예를 들어, 다음과 같이 "substring" 함수를 사용하면 특정 범위의 문자열을 추출할 수 있습니다.

```Elm
substring : Int -> Int -> String -> String
```

이 함수는 문자열의 시작 인덱스와 끝 인덱스를 인자로 받습니다. 따라서 우리는 이 함수를 활용해 일치하는 패턴의 문자를 삭제할 수 있습니다. 예를 들어, 다음과 같은 문자열이 있다고 가정해 봅시다.

```Elm
let str = "Elm 프로그래밍은 재미있다!"
```

그리고 우리가 "프로그래밍은" 부분을 삭제하고 싶다면, 다음과 같이 코드를 작성할 수 있습니다.

```Elm
substring 0 7 str ++ substring 14 (String.length str) str
```

이 코드는 먼저 문자열의 인덱스 0부터 6까지 추출합니다. 그리고 그 뒤에는 인덱스 14부터 문자열의 끝까지 추출합니다. 따라서 결과적으로 우리는 "Elm은 재미있다!"라는 새로운 문자열을 얻게 됩니다.

이와 같은 방식으로, 우리는 원하는 대로 문자열에서 일치하는 패턴의 문자를 삭제할 수 있습니다.

## 깊이 들어가기

"substring" 함수를 사용하는 것 외에도, Elm 언어에는 문자열 조작을 위한 다양한 함수들이 있습니다. 예를 들어, "replace" 함수를 사용하면 특정 패턴을 지정한 문자열로 바꿀 수 있습니다.

또한, 정규식(Regular Expression)을 활용하면 더욱 다양한 패턴의 문자를 삭제할 수 있습니다. 정규식은 복잡한 패턴을 표현할 수 있는 문자열 패턴 매칭 기능입니다.

따라서 상황과 요구에 따라 적절한 함수나 기술을 활용하여 문자열에서 일치하는 패턴의 문자를 삭제하는 것이 중요합니다.

## 더 보기

- [Elm 공식 문서: String Operations](https://package.elm-lang.org/packages/elm/core/latest/String#operations)
- [MDN: Regular Expression](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)