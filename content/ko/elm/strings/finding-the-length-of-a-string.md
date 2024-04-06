---
date: 2024-01-20 17:47:41.513720-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD560\uAE4C?) Elm\uC5D0\uC11C \uBB38\uC790\
  \uC5F4 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740 `String.length` \uD568\uC218\
  \uB85C \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uCF54\uB4DC \uC608\uC81C\uC640 \uACB0\uACFC\
  \uB294 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.854510-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD560\uAE4C?) Elm\uC5D0\uC11C \uBB38\uC790\uC5F4 \uAE38\
  \uC774\uB97C \uCC3E\uB294 \uAC83\uC740 `String.length` \uD568\uC218\uB85C \uAC04\
  \uB2E8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (어떻게 할까?)
Elm에서 문자열 길이를 찾는 것은 `String.length` 함수로 간단합니다. 코드 예제와 결과는 다음과 같습니다.

```Elm
lengthOfString : String -> Int
lengthOfString str =
    String.length str

-- "Hello, World!" 의 길이는 13입니다.
lengthOfHelloWorld : Int
lengthOfHelloWorld =
    lengthOfString "Hello, World!"
```

## Deep Dive (심층 분석)
문자열의 길이를 결정하는 것은 컴퓨팅의 오래된 문제입니다. 예전에는 문자열이 고정 너비의 배열로 저장되곤 했으나, 현대 언어들은 동적 할당을 제공합니다. Elm에서는 UTF-16을 사용해 문자열을 표현하므로, 특정 언어의 문자가 더 많은 공간을 차지하게 됩니다. `String.length`는 문자열에 있는 "code units"의 수를 반환하나, 이것은 실제 "문자" 수와 다를 수 있습니다. 실제 문자 수를 알고 싶다면, `Grapheme` 패키지를 고려해볼 수 있습니다.

대안적으로, 문자열은 문자의 연속으로 생각할 수 있으며, 각 문자를 순회할 수도 있습니다. 하지만, 이 방법은 비효율적일 수 있으므로 일반적으로는 `String.length`를 사용하는 것이 좋습니다. Elm의 문자열 내부 구현은 성능과 정확성 사이의 균형을 맞추려고 노력합니다.

## See Also (참고자료)
- Elm의 공식 문자열 패키지 문서: [String package](http://package.elm-lang.org/packages/elm/core/latest/String)
- 문자열의 길이를 다루는 데 있어서 Unicode 처리에 대한 자세한 정보: [Unicode in Elm](https://package.elm-lang.org/packages/elm-lang/core/latest/Char#unicode)
- Elm 한글 사용자 그룹 또는 포럼에서의 토론: Elm Korea [Google Group](https://groups.google.com/forum/#!forum/elm-discuss)
