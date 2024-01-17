---
title:                "문자열 추출하기"
html_title:           "Elm: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

뭐 & 왜?

 문자열의 일부를 추출하는 것은 간단한 작업처럼 보이지만, 그것이 프로그래밍에서 중요한 이유가 있습니다. 문자열 추출은 일반적으로 원하는 정보를 다룰 때 필요하고, 작업을 더 효율적으로 만들 수 있는 방법으로 사용됩니다.

어떻게:

Elm은 문자열 추출을 처리하기 위해 `String.slice` 함수를 제공합니다. 이 함수는 문자열에서 시작 인덱스와 끝 인덱스를 지정하여 추출할 부분 문자열을 선택합니다. 예를 들어, 다음과 같은 코드를 작성하면 "world" 문자열이 추출됩니다.

```Elm
let
  str = "hello, world!"
  result = String.slice 7 12 str
in
  result
```
출력:

"world"

더 깊게 들어가보면:

문자열 추출 기능은 다양한 언어에서 사용 가능하지만, 각각 다른 방식으로 구현되고 있습니다. 일부 언어에서는 `substring` 함수를 사용하며, 다른 언어에서는 문자열을 배열로 변환한 다음 원하는 부분만 선택하는 방식을 사용합니다. Elm에서는 `String.Slice` 함수의 경우 시작과 끝 인덱스가 반드시 해당 문자열의 범위 내에 있어야 한다는 제한적인 적용 범위가 있습니다.

관련 자료:

- [Elm 문자열 관련 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [자바스크립트에서 문자열 추출 방법 비교](https://www.geeksforgeeks.org/javascript-string-slice-vs-substring-vs-substr/#:~:text=extract%20characters%20from%20a%20string,start%20to%20the%20end%20string)
- [다양한 언어에서의 문자열 추출 기능 비교](https://stackoverflow.com/questions/2243824/in-built-string-extraction-method-in-different-languages)