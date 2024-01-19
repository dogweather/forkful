---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

서브스트링 추출은 문자열의 특정 부분을 나누어 내는 것을 의미합니다. 프로그래머들은 데이터 분석, 검색 기능 구현, 문자열 처리 등에 서브스트링 추출을 활용합니다.

## 어떻게:
```elixir
defmodule MyString do
  def substring(input, start, length) do
    String.slice(input, start, start + length)
  end
end

IO.puts(MyString.substring("Elixir Programming", 7, 9))
```
이 코드를 실행시키면 아래와 같은 결과를 얻을 수 있습니다.
```
Programming
```
## Deep Dive:

서브스트링 추출 기능은 처음에는 C언어 등의 낮은 수준의 언어에서 주로 사용되었습니다. 이런 기능이 높은 수준의 언어에 도입된 것은 상대적으로 최근의 일입니다. Elixir에서는 `String.slice/3` 함수를 통해 서브스트링을 추출할 수 있습니다. 
다른 방식으로는 정규표현식을 사용하는 방법이 있습니다. 하지만 이 방법은 복잡하므로, 간단한 작업에는 `String.slice/3` 함수를 사용하는 것이 좋습니다.

## 함께 보기:
Elixir 공식 문서에서 자세한 내용을 확인하실 수 있습니다.
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Learning Elixir Documentation](https://elixirschool.com/en/lessons/basics/strings/)