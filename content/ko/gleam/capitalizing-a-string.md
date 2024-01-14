---
title:    "Gleam: 문자열 대문자 변환하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 변환하는 것이 왜 중요한지 궁금하신가요? 그렇다면 이 블로그 포스트를 읽어보세요!

## 어떻게

Gleam에서 문자열의 첫 글자를 대문자로 변환하는 방법은 아주 간단합니다. 다음과 같이 코드를 작성하세요.

```Gleam
let str = "hello"
let new_str = String.capitalize(str)
```

위 코드를 실행하면 `new_str` 변수에는 "Hello"라는 값이 저장됩니다. 이렇게 간단하게 문자열의 첫 글자를 대문자로 변환할 수 있습니다.

## 깊게 들어가보기

실제로 Gleam의 `String.capitalize` 함수는 어떻게 구현되어 있는 걸까요? 이 함수는 문자열을 받아서 첫 글자를 대문자로 변환하고 나머지는 그대로 유지하는 방식으로 동작합니다. 이를 위해 문자열의 첫 글자를 먼저 대문자로 변환하고, 나머지 부분은 `String.slice` 함수를 사용해 잘라내어 이어 붙이는 방식으로 구현되어 있습니다. Gleam 소스 코드를 확인해보면 더 자세한 내용을 확인할 수 있습니다.

## 더 알아보기

이외에도 Gleam에서는 다양한 문자열 처리 관련 함수를 제공하고 있습니다. 이 함수들을 살펴보고 싶다면 아래 링크들을 참고해보세요.

- [Gleam 공식 문서 - 문자열](https://gleam.run/documentation/stdlib/string/)
- [Gleam GitHub 리포지토리](https://github.com/gleam-lang/gleam)
- [Gleam 문자열 관련 소스 코드](https://github.com/gleam-lang/gleam/blob/main/lib/gleam_string/src/String.gleam)

## 관련 링크

- [Gleam 공식 웹사이트](https://gleam.run)
- [Gleam 공식 GitHub 리포지토리](https://github.com/gleam-lang/gleam)
- [Gleam 공식 트위터 계정](https://twitter.com/gleamlang)