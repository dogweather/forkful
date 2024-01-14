---
title:    "Gleam: 문자열 대문자로 바꾸기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
본격적인 사람, 언어 또는 프로그램은 모두 대문자로 된 문자열을 다룬다는 것을 알고 있습니다. 왜냐하면 보통 프로그래밍에서, 대문자로된 문자열은 특수한 경우에 사용될 수 있지만, 보통은 두꺼운 쉼표, 큰 따옴표 등에서 사용되기 때문입니다.

## 하는 법
```Gleam
fn capitalize_words(string) {
  // 문자열을 대문자로 변환
  let upper_string = string |> String.uppercase
  // 각 단어를 리스트로 분리
  let words = upper_string |> String.split([32])
  // 첫 번째 문자만 대문자로 변경
  let capitalized_words =
  words
  |> List.map(fn(word) {
    word
    |> String.split_at(1)
    |> fn(ar) { String.join(ar) }
    |> String.uppercase
  })
  // 최종 결과는 리스트로 반환
  capitalized_words
}
```

- 입력: "hello world"
- 출력: ["Hello", "World"]

## 깊은 이해
대문자로 된 문자열을 다루는 것의 깊은 이해는 프로그래밍에서 매우 중요합니다. 이것은 보통 글의 제목이나 변수의 이름 등을 다룰 때 유용합니다. 또한 대문자로 된 문자열을 다루기 위한 다른 방법도 있습니다. 예를 들어 `String.capitalize()` 함수가 있으며, 이 함수에서는 첫 번째 문자만 대문자로 바꿔주는 것이 아니라 전체 문자열을 대문자로 바꿔줍니다. 이를 통해 임의의 문자열에 대해 대문자를 적용할 수 있습니다.

## 관련 링크
- [Gleam 공식 웹사이트](https://gleam.run/)
- [Gleam Github 저장소](https://github.com/gleam-lang/gleam)
- [Gleam 강의 「Gleam을 러스도 지정하고 있다」](https://www.youtube.com/watch?v=RiC-ZxBeLG8)
- [Gleam 관련 블로그 포스트 모음](https://medium.com/tag/gleam)
- [Gleam 에러 코드 목록](https://github.com/gleam-lang/gleam/blob/master/lib/error.gleam)