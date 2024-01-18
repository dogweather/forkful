---
title:                "문자열에서 날짜 분석하기"
html_title:           "Elixir: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
문자열에서 날짜를 파싱한다는 것은 문자열에서 날짜 형식을 읽어서 프로그래머가 원하는 형식으로 변환하는 것입니다. 이를 통해 사용자 입력이나 외부 API에서 받은 데이터를 더 쉽게 처리할 수 있습니다.

## 방법:
주어진 문자열에서 날짜를 파싱하는 가장 간단한 방법은 Elixir의 `Date` 모듈을 사용하는 것입니다. `Date.from_iso8601` 함수를 사용하면 ISO 8601 형식의 문자열을 날짜로 변환할 수 있습니다.

```Elixir
date_string = "2021-10-05"
date = Date.from_iso8601(date_string)
IO.inspect date

# Output:
# ~D[2021-10-05]
```

더 복잡한 날짜 형식을 파싱하려면 정규표현식을 사용할 수도 있습니다. Elixir의 `Regex` 모듈을 활용하면 자유롭게 문자열을 분리하고 날짜로 변환할 수 있습니다.

```Elixir
date_string = "October 5th, 2021"
{:ok, [_, month, day, _, year]} = Regex.run(~r/\A(\w+)\s(\d+)[a-z]+,\s(\d+)\z/, date_string)
{month, valid} = String.to_integer(month)
{day, valid} = String.to_integer(day)
{year, valid} = String.to_integer(year)
date = Date.new(year, month, day)
IO.inspect date

# Output:
# ~D[2021-10-05]
```

## 깊게 들어가기:
프로그래밍에서 날짜를 파싱하는 것은 오래전부터 널리 사용되어 온 기법입니다. 이를 통해 다양한 형식의 날짜를 동일한 형식으로 통일할 수 있으며, 이는 다른 프로그램과의 호환성을 높여줍니다. 또한 Elixir의 `DateTime` 모듈을 사용하면 시간대나 시간을 고려한 날짜 처리도 가능합니다.

다른 대안으로는 `Calendar` 모듈의 `Date` 타입을 사용하는 것이 있습니다. 이를 활용하면 더 많은 날짜 관련 기능을 사용할 수 있습니다.

## 관련 자료:
- [Elixir Date 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir Regex 문서](https://hexdocs.pm/elixir/Regex.html)
- [Elixir Calendar 문서](https://hexdocs.pm/elixir/Calendar.html)