---
date: 2024-01-20 17:47:41.513720-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740\
  \ \uAC04\uB2E8\uD788 \uBB38\uC790\uC5F4\uC774 \uBA87 \uAC1C\uC758 \uBB38\uC790\uB85C\
  \ \uC774\uB8E8\uC5B4\uC838 \uC788\uB294\uC9C0 \uC54C\uC544\uB0B4\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\
  \ \uAC80\uC99D, \uD14D\uC2A4\uD2B8 \uCC98\uB9AC, \uC0AC\uC6A9\uC790 \uC785\uB825\
  \ \uAD00\uB9AC \uB4F1\uC744 \uC704\uD574 \uC774 \uC815\uBCF4\uB97C \uD544\uC694\uB85C\
  \ \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.010201
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740 \uAC04\
  \uB2E8\uD788 \uBB38\uC790\uC5F4\uC774 \uBA87 \uAC1C\uC758 \uBB38\uC790\uB85C \uC774\
  \uB8E8\uC5B4\uC838 \uC788\uB294\uC9C0 \uC54C\uC544\uB0B4\uB294 \uACFC\uC815\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uAC80\
  \uC99D, \uD14D\uC2A4\uD2B8 \uCC98\uB9AC, \uC0AC\uC6A9\uC790 \uC785\uB825 \uAD00\uB9AC\
  \ \uB4F1\uC744 \uC704\uD574 \uC774 \uC815\uBCF4\uB97C \uD544\uC694\uB85C \uD569\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열의 길이를 찾는 것은 간단히 문자열이 몇 개의 문자로 이루어져 있는지 알아내는 과정입니다. 프로그래머들은 데이터 검증, 텍스트 처리, 사용자 입력 관리 등을 위해 이 정보를 필요로 합니다.

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
