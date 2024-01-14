---
title:    "Elm: 문자열 연결하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 대해 생각해보았을 때, 다른 언어에서 사용하는 문자열 결합 연산자 (concatenation operator)와 동일해 보이기 때문에 인상적이지 않을 수 있습니다. 그러나 문자열을 연결하는 것은 실제로 매우 중요하고 유용한 기능입니다. 문자열을 연결하면 기존의 문자열에 새로운 값을 추가하여 더 큰 문자열을 만들 수 있습니다. 예를 들어, 사용자의 이름과 환영 메시지를 함께 표시할 수 있습니다.

## 어떻게

우리는 Elm에서 2가지 방법을 사용하여 문자열을 연결할 수 있습니다.

```Elm
-- 첫번째 방법: ++ 연산자 사용하기
greeting = "안녕하세요, "
name = "John"
message = greeting ++ name -- "안녕하세요, John"

-- 두번째 방법: String.concat 함수 사용하기
greeting = "Hello, "
name = "Jane"
message = String.concat [greeting, name] -- "Hello, Jane"
```

먼저 `++` 연산자를 사용하여 두 개의 문자열을 이어붙일 수 있습니다. 또한 `String.concat` 함수를 사용하여 문자열의 배열을 이어 붙일 수도 있습니다. 이를 사용하면 여러 개의 문자열을 이어붙일 수 있습니다. `++` 연산자를 사용하는 것이 더 간편하고 직관적이지만, 여러 개의 문자열을 이어붙일 때는 `String.concat` 함수가 더 편리합니다.

위의 예시들에서는 미리 정의된 문자열 변수를 사용하였지만, 이 두 방법은 어떤 문자열이든 이어붙일 수 있으므로 사용자 입력에 따라 유연하게 적용할 수 있습니다.

## Deep Dive

문자열을 연결하는 것은 실제로 매우 효율적입니다. Elm에서 문자열은 내부적으로 다른 문자열을 포함하는 배열로 표현됩니다. 여러 개의 문자열을 이어붙일 때, 새로운 문자열이 새로 할당되는 것이 아니라 기존의 배열에 문자열이 추가됩니다. 이를 통해 연결하는 데에 필요한 메모리가 최소화되며, 처리 속도가 빠릅니다.

또한, Elm에는 `String.join`이라는 함수도 있습니다. 이 함수는 주어진 구분자를 사용하여 배열에 있는 모든 문자열을 이어붙입니다. 예를 들어, `String.join "-" ["elm", "programming", "blog"]`을 실행하면 "elm-programming-blog"이라는 문자열이 반환됩니다.

## See Also

- [Elm 공식 문서 - String 모듈](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 공식 문서 - 문장 포매팅](https://guide.elm-lang.org/interop/formatting.html)
- [Elm에 대한 자세한 정보와 예제](https://elmprogramming.com/)