---
title:                "부분 문자열 추출"
html_title:           "Elixir: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
때로는 문자열에서 일부분을 추출하여 작업하는 것이 필요한 경우가 있습니다. 이런 경우에는 Elixir의 서브스트링 추출 기능이 매우 유용합니다.

## 어떻게
서브스트링을 추출하기 위해서는 두 가지 방법이 있습니다. 첫 번째 방법은 `String.slice/2` 함수를 사용하는 것입니다. 이 함수는 두 개의 인수를 받아 첫 번째 인수에서 두 번째 인수로 지정한 문자 사이의 부분 문자열을 추출합니다. 예를 들어:

```
Elixir iex> String.slice("안녕하세요", 0, 2)
"안녕"
```

두 번째 방법은 `String.split/3` 함수를 사용하는 것입니다. 이 함수는 세 개의 인수를 받아 첫 번째 인수에서 두 번째 인수로 지정한 패턴을 기준으로 부분 문자열을 나누고, 세 번째 인수는 반환된 문자열 배열에서 무시할 공백 문자의 수를 나타냅니다. 예를 들어:

```
Elixir iex> String.split("Hello, world!", ",", trim: true)
["Hello", "world!"]
```

## 딥 다이브
서브스트링 추출은 다양한 방식으로 활용될 수 있습니다. 예를 들어, `String.trim/1` 함수를 사용하여 문자열의 앞뒤 공백을 제거하고 `String.length/1` 함수를 사용하여 문자열의 길이를 확인한 후, 필요한 부분만 추출할 수 있습니다. 또는 `Regex` 모듈을 사용하여 정규식을 이용해 문자열을 추출할 수도 있습니다. 자세한 내용은 Elixir 문서나 인터넷 자료를 참고하세요.

## 더 알아보기
* [Elixir 문서 - String 모듈](https://hexdocs.pm/elixir/String.html)
* [Elixir 문서 - Regex 모듈](https://hexdocs.pm/elixir/Regex.html)
* [블로그 - Elixir에서 문자열 다루기](https://blog.process-one.net/elixir-string-operations/)