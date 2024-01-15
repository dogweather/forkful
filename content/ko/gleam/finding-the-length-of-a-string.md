---
title:                "문자열의 길이 찾기"
html_title:           "Gleam: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 길이를 찾는 것에 대해 왜 관심이 있을까요? 문자열은 프로그래밍에서 매우 중요하며, 이를 다루는 작업은 매우 일상적입니다. 문자열의 길이를 찾는 것은 프로그래머에게 매우 유용한 작업입니다. 

## 방법
문자열의 길이를 찾는 방법은 간단합니다. 먼저 문자열을 변수에 할당한 다음, 해당 변수를 `length()` 함수에 전달하면 문자열의 길이를 반환합니다. 아래의 Gleam 코드 예제를 참고해주세요.

 ```Gleam
let my_string = "Hello World!"
let string_length = length(my_string)
```

위의 코드를 실행하면 `string_length` 변수에는 `12` 값이 할당될 것입니다. 이렇게 간단한 방법으로 문자열의 길이를 찾을 수 있습니다. 

## 깊이 파고들기
`length()` 함수는 Gleam의 기본 라이브러리중 하나이며, 내부로 들어가보면 확장성 있는 코드로 구성되어 있습니다. 이 함수는 `String.length()` 모듈을 호출하고 이 모듈은 문자열의 길이를 확인하기 위해 `Str.length()` 함수를 호출합니다. 이렇게 층층이 확장되는 구조로 이루어져 있지만, 간단하고 유지보수하기 쉬운 방식으로 구현되어 있습니다.

## 참고 자료
- [Gleam Documentation](https://gleam.run/)
- [Learn Gleam in Y minutes](https://learnxinyminutes.com/docs/intro-to-gleam/)
- [Introduction to Gleam: A Functional Language on the BEAM](https://www.sitepoint.com/functional-language-gleam-beam/)