---
title:    "Gleam: 문자열의 길이 찾기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜 이것을 해야 할까?

문자열의 길이를 찾는 작업은 프로그래밍에서 자주 사용되는 기본적인 작업입니다. 예를 들어, 문자열에 포함된 단어의 개수를 세거나, 입력된 이메일이 유효한 형식인지를 확인하는 등의 작업에서 문자열의 길이를 알아야 할 수 있습니다. 따라서 이번 블로그 포스트에서는 Gleam을 이용하여 문자열의 길이를 찾는 방법에 대해 알아보겠습니다.

## 어떻게 하는가?

문자열의 길이를 알아내는 가장 간단한 방법은 `String.length` 함수를 사용하는 것입니다. 이를 통해 문자열의 길이를 반환할 수 있습니다. 아래는 Gleam을 이용한 예시 코드와 그 실행 결과를 보여줍니다.

```Gleam
let string = "Hello, world!"

let string_length = String.length(string)

// 13 출력
```

위의 코드에서 `String.length` 함수를 호출하여 문자열의 길이를 반환하고, 이를 `string_length` 변수에 할당한 후 출력하도록 했습니다.

## 깊이 파고들어보기

문자열의 길이를 찾는 방법에 대해 좀 더 깊이 있게 알아보겠습니다. Gleam에서는 문자열이 아닌 다른 자료형을 문자열로 변환해주는 기능도 있습니다. `String.from` 함수를 이용하면 정수형을 문자열로 변환할 수 있습니다. 아래는 이를 이용한 예시 코드입니다.

```Gleam
let integer = 123

let string = String.from(integer)

// "123" 출력
```

위의 예시 코드에서 `String.from` 함수를 이용하여 정수형을 문자열로 변환한 후 출력하도록 했습니다.

## 참고자료

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Gleam 공식 GitHub 리포지터리](https://github.com/gleam-lang/gleam)
- [Gleam 튜토리얼 비디오](https://www.youtube.com/watch?v=m8E44HPLe98)

## 더 보기

- [Gleam으로 만드는 상호 작용형 웹 애플리케이션](https://wiki.haskellers.com/Lectures/Haskell/Tutorials/Client-side_web_applications_with_ghcjs_and_refs_in_web_browser)
- [Gleam으로 구현한 간단한 수열 계산기](https://github.com/ostera/engineering-practice/blob/30b891846217b17a50d555cf6937b8c7703f3998/source/week2.md#gleam)
- [Gleam으로 구현한 RESTful API 서버 스케치](https://gist.github.com/sasa1978/eaf70124fb0c2b9e1e3d1ba5b236f1b9)