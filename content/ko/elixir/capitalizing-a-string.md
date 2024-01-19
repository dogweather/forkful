---
title:                "문자열 대문자화"
html_title:           "Elixir: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜? 

문자열을 대문자로 바꾸는 것은 모든 첫 글자를 대문자로 만드는 것을 의미합니다. 이렇게하는 이유는 대개 텍스트를 명확하게 구분하고, 읽기 쉽게 만들기 위해서입니다.

## 방법:

Elixir의 String 모듈에서 제공하는 `capitalize/2` 함수를 사용하여 문자열을 대문자로 바꿀 수 있습니다. 아래에 코드 예시를 참고해보세요.

```Elixir
IO.puts String.capitalize("elixir programming")
```

위 코드의 출력은 다음과 같습니다:

```Elixir
Elixir programming
```

## 깊이 들여다보기:

사실, 과거의 프로그래밍 언어들은 대소문자 변환 기능을 내장하고 있지 않았습니다. 이 기능이 생겨난 이유는 위에서 언급한 것처럼 텍스트를 더욱 명확하게 구분하고 읽기 쉽게 만들기 위해서입니다. 

대안으로 `upcase/1`와 `downcase/1` 함수를 사용할 수도 있지만, 이러한 함수들은 문자열의 모든 글자를 대문자 혹은 소문자로 바꿉니다. 

`capitalize/2` 함수는 첫 글자만 대문자로 바꾸고, 나머지 글자들은 소문자로 변경합니다. 이 함수의 두번째 매개변수로 언어를 지정할 수 있으며, 기본값은 언어 기본 설정입니다.

## 참고 자료:

1. Elixir 공식 문서: [String.capitalize/2](https://hexdocs.pm/elixir/String.html#capitalize/2)
2. Elixir School: [Strings](https://elixirschool.com/en/lessons/basics/strings/)