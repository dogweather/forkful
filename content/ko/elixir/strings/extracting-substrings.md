---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:45:56.966584-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
서브스트링 추출이란 무엇일까요? 간단히 말해서, 문자열 일부를 뽑아내는 것입니다. 프로그래머는 특정 데이터를 다룰 때, 정보의 일부분만 필요로 하기 때문에 이 작업을 합니다.

## How to:
Elixir에서는 String 모듈을 사용해 서브스트링을 쉽게 추출할 수 있어요. 아래 예시를 확인해 보세요.

```elixir
# 기본적인 추출
string = "Hello, Seoul!"
substring = String.slice(string, 7, 5)
IO.puts(substring) # "Seoul"

# 음수 인덱스와 한글 사용
korean_string = "안녕하세요, 서울!"
korean_substring = String.slice(korean_string, -3, 2)
IO.puts(korean_substring) # "서울"

# 패턴을 이용한 추출
pattern_substring = String.split(korean_string, ",") |> List.last() |> String.trim()
IO.puts(pattern_substring) # "서울!"
```

## Deep Dive
Elixir에서 서브스트링을 추출하는 기능은 기존 Erlang VM의 효율적인 바이너리 처리 능력에 기반합니다. 이는 Elixir의 성능이 Erlang의 강력함을 계승한다는 것을 의미해요. 문자열은 UTF-8 인코딩으로 바이너리로 관리되며, 이 덕분에 다국어 문자열 처리가 가능합니다. `String.slice/3` 함수를 통해 간단하게 서브스트링을 가져올 수 있지만, 문자열 패턴 매칭을 통해서도 복잡한 로직의 서브스트링 추출이 가능해요.

Python이나 JavaScript에도 비슷한 기능들이 있지만, Elixir는 함수형 프로그래밍의 이점을 살려 간결하고 명확한 코드 작성이 가능하게 해줍니다. 불변성(immutable)의 원칙에 따라 원본 문자열이 변경되지 않고 새로운 문자열이 반환된다는 것을 기억하세요.

## See Also
문자열 처리에 대한 더 깊은 이해를 위해 Elixir 공식 문서를 참조하면 좋습니다:

- Elixir 공식 문서의 String 모듈: [String — Elixir v1.12.3](https://hexdocs.pm/elixir/String.html)
- 문자열 관련 레시피를 담은 'Elixir School': [Elixir School - Strings](https://elixirschool.com/en/lessons/basics/strings/)

이러한 자료들은 Elixir로 문자열을 다루는 다양한 방법을 제공하여 여러분의 프로그래밍 기술을 더욱 향상시켜 줄 것입니다.
