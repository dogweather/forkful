---
title:                "Elm: 문자열을 소문자로 변환"
simple_title:         "문자열을 소문자로 변환"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜?

 Elm에서 문자열을 소문자로 변환하는 것이 왜 중요한지 궁금하신가요? 문자열을 다루는 컴퓨터 프로그래밍에서 대소문자는 매우 중요한 역할을 합니다. 예를 들어, 사용자의 입력이나 데이터베이스에서 가져온 값을 비교할 때, 대소문자를 구분하면 정확한 결과를 얻을 수 있습니다. 따라서 문자열을 소문자로 변환하는 작업은 프로그래밍에서 매우 중요한 기능 중 하나입니다.

## 어떻게?

 Elm의 `String` 모듈에는 문자열을 소문자로 변환하는 유용한 함수인 `toLower`가 있습니다. 이 함수를 사용하기 위해서는 먼저 `import` 섹션에서 `String` 모듈을 불러와야 합니다.
 
 ```Elm
 import String
 
 lowerCaseString : String
 lowerCaseString = String.toLower "ELM PROGRAMMING"
 
 main : Html msg
 main =
   text lowerCaseString
 ```
 
 위 코드에서 `toLower` 함수는 대문자로 이루어진 문자열을 소문자로 변환하여 `lowerCaseString` 변수에 대입한 후, `text` 함수를 사용하여 웹 페이지에 결과를 출력합니다. 아래는 이 코드의 실행 결과입니다.
 
 ``` bash
 "elm programming"
 ```

## 더 깊게

 `toLower` 함수는 단순히 대문자를 찾아 소문자로 변환하는 것만으로 역할을 마치지 않습니다. 이 함수는 유니코드 문자열을 입력받고 유니코드 단위로 문자의 대소문자를 구분하여 변환합니다. 또한 `toLower` 함수는 변환 결과가 원래의 입력과 달라질 수 있습니다. 예를 들어, `ı` (LATIN SMALL LETTER DOTLESS I) 글자는 `i` (LATIN SMALL LETTER I)로 변환됩니다. 따라서 변환 결과를 반드시 확인하고 이를 적절하게 처리하는 것이 중요합니다.

## 비슷한 주제

- [Elm `String` 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm 문자열 다루기: 문자열 조작하기](https://korean-elm.ninjamock.com/playground/application-structure/content/1)