---
title:                "정규 표현식 사용하기"
aliases: - /ko/elixir/using-regular-expressions.md
date:                  2024-02-03T19:16:43.154783-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

엘릭서(Elixir)에서 정규 표현식(regex)은 특정 패턴을 기반으로 문자열을 검색, 일치 시키고, 조작하기 위해 사용됩니다. 프로그래머들은 문자열 처리에서의 효율성과 다양성 덕분에, 정규 표현식을 이메일, URL과 같은 형식의 유효성 검사, 로그 파싱, 데이터 추출과 같은 작업에 활용합니다.

## 사용 방법:

엘릭서는 정규 표현식 작업에 에를랑(Erlang)의 정규 표현식 라이브러리를 활용하는 `Regex` 모듈을 사용합니다. 기본적인 사용법은 다음과 같습니다:

```elixir
# 패턴 일치 - 첫 번째 일치 항목 반환
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # 출력: ["hello"]

# 모든 일치 항목 찾기
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # 출력: [["2"], ["5"]]

# 문자열의 일부 교체
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # 출력: "Elixir_is_fun"
```

더 복잡한 패턴과 기능을 위해서는 타사 라이브러리를 사용할 수도 있지만, 대부분의 핵심 문자열과 패턴 매칭 작업에 대해 엘릭서의 내장 `Regex` 모듈은 매우 강력합니다.

대소문자를 구분하지 않는 일치를 수행하려면 `i` 옵션을 사용하세요:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # 출력: ["Hello"]
```

여러 번 사용될 때 효율성을 위해 정규 표현식을 미리 컴파일 할 수 있습니다:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # 출력: ["hello"]
```

엘릭서는 또한 코드를 더 읽기 쉽게 만들면서 문자열의 특정 부분을 추출하는 데 매우 유용한 명명된 캡처를 지원합니다:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # 출력: %{"year" => "2023", "month" => "04", "day" => "15"}
```

이 간단한 개요는 엘릭서가 정규 표현식을 얼마나 쉽게 다루는지, 강력한 문자열 조작 및 데이터 추출 기술을 가능하게 하는지를 강조합니다.
