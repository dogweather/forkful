---
date: 2024-01-20 17:45:56.966584-07:00
description: "\uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC774\uB780 \uBB34\uC5C7\
  \uC77C\uAE4C\uC694? \uAC04\uB2E8\uD788 \uB9D0\uD574\uC11C, \uBB38\uC790\uC5F4 \uC77C\
  \uBD80\uB97C \uBF51\uC544\uB0B4\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB294 \uD2B9\uC815 \uB370\uC774\uD130\uB97C \uB2E4\uB8F0 \uB54C, \uC815\
  \uBCF4\uC758 \uC77C\uBD80\uBD84\uB9CC \uD544\uC694\uB85C \uD558\uAE30 \uB54C\uBB38\
  \uC5D0 \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:51.742682-07:00'
model: gpt-4-1106-preview
summary: "\uC11C\uBE0C\uC2A4\uD2B8\uB9C1 \uCD94\uCD9C\uC774\uB780 \uBB34\uC5C7\uC77C\
  \uAE4C\uC694? \uAC04\uB2E8\uD788 \uB9D0\uD574\uC11C, \uBB38\uC790\uC5F4 \uC77C\uBD80\
  \uB97C \uBF51\uC544\uB0B4\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB294 \uD2B9\uC815 \uB370\uC774\uD130\uB97C \uB2E4\uB8F0 \uB54C, \uC815\uBCF4\
  \uC758 \uC77C\uBD80\uBD84\uB9CC \uD544\uC694\uB85C \uD558\uAE30 \uB54C\uBB38\uC5D0\
  \ \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
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
