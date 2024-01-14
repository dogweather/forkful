---
title:    "Elm: 문자열을 소문자로 변환하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# 왜
문자열을 소문자로 변환하는 것에 참여하는 이유는 간단합니다. 데이터의 일관성을 유지하기 위해서입니다. 일관된 형식으로 문자열을 다루는 것은 더 나은 코드를 작성하는데 도움이 될 수 있습니다.

## 어떻게
```elm
toLower : String -> String
toLower "HELLO" --> "hello"
```

우선, 우리는 Elm의 `toLower` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 이 함수는 문자열을 새로운 소문자로 변환된 문자열로 바꾸어주는 것입니다. 이렇게 하면 데이터의 일관성이 유지되며 작업 중 발생할 수 있는 오류를 방지할 수 있습니다.

```elm
String.toLower "WORLD" --> "world"
```

또 다른 방법은 `String.toLower` 함수를 사용하는 것입니다. 이 함수는 문자열을 소문자로 변환하여 새로운 문자열로 반환합니다. 위의 예제와 같이 사용할 수 있습니다. 어떤 방법을 사용하더라도 원하는 결과를 얻을 수 있습니다.

## 딥 다이브
`toLower` 함수와 `String.toLower` 함수는 문자열을 소문자로 변환하는 데 옵션을 제공합니다. 예를 들어, 아티클 제목에 사용되는 단어의 첫 글자를 대문자로 변환하고 싶은 경우, `toLower` 함수를 사용하여 전체 문자열을 소문자로 변환한 다음 `String.capitalize` 함수를 사용하여 첫 글자를 대문자로 변환할 수 있습니다.

```elm
String.capitalize (toLower "HELLO WORLD") --> "Hello world"
```

또한, Elm의 `String` 모듈에는 문자열의 특정 부분을 소문자로 변환하는 함수도 있습니다. 특정 시작 인덱스와 끝 인덱스를 지정하여 부분 문자열을 소문자로 변환할 수 있습니다.

```elm
String.toLowerRange 5 8 "Hello WORLD" --> "Hello world"
```

문자열을 소문자로 변환하는 방법은 다양합니다. 우리는 문자열을 다룰 때 이러한 함수들을 사용하여 일관성 있는 데이터를 유지할 수 있습니다.

# 참고
[Elm 공식 문서](https://guide.elm-lang.org/strings/)에서 문자열을 다루는 다양한 함수를 알아볼 수 있습니다. 또한 Elm에서 사용 가능한 [기타 문자열 관련 라이브러리](https://package.elm-lang.org/packages/elm/core/latest/String)도 살펴볼 수 있습니다.