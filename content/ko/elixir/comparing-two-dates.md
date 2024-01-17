---
title:                "두 날짜 비교하기."
html_title:           "Elixir: 두 날짜 비교하기."
simple_title:         "두 날짜 비교하기."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
두 날짜를 비교하는 것은 어떤 날짜가 더 이른지 또는 늦은지를 알아내는 것을 말합니다. 프로그래머들은 이 작업을 왜 할까요? 컴퓨터 시스템에는 많은 데이터가 저장되어 있는데, 이 데이터들 중에서 날짜도 매우 중요한 정보입니다. 그래서 두 날짜를 비교하면 더욱 더 정확한 데이터 처리가 가능해집니다.

## 방법:
```Elixir
# 두 날짜가 같은지 비교
Date.compare(Date.today(), Date.today())

# 두 날짜가 이전인지 비교
Date.compare(Date.new(2020, 12, 31), Date.new(2021, 1, 1))

# 두 날짜가 이후인지 비교
Date.compare(Date.new(2021, 1, 1), Date.new(2020, 12, 31))

# 두 날짜가 이전, 이후 또는 같음을 나타내는 열거형 데이터 조회
comparison_result = Date.compare(Date.new(2020, 1, 1), Date.new(2021, 1, 1))
IO.puts("두 날짜 비교 결과: #{comparison_result}")
```

위의 예제 코드를 실행하면 두 날짜를 비교하여 결과를 반환하는 것을 볼 수 있습니다. 또한 반환된 결과를 활용하여 두 날짜의 관계를 알 수 있습니다.

## 깊이 들어가보기:
- 비교하는 날짜의 형식이 유효한지 검사하는 것이 중요합니다. Elixir에서는 [Date.parse/1](https://hexdocs.pm/elixir/Date.html#parse/1) 함수를 통해 날짜를 파싱하여 유효성을 검사할 수 있습니다.
- 두 날짜를 비교하는 방법에는 다양한 방식이 있습니다. 위의 예제에서는 [Date.compare/3](https://hexdocs.pm/elixir/Date.html#compare/3) 함수를 사용했지만, [Calendar.ISO](https://hexdocs.pm/elixir/Calendar.ISO.html) 모듈의 함수를 통해 더 다양한 비교 방식을 사용할 수 있습니다.
- Elixir에서 날짜와 관련된 작업을 할 때는 [Date](https://hexdocs.pm/elixir/Date.html) 모듈 외에도 [DateTime](https://hexdocs.pm/elixir/DateTime.html), [NaiveDateTime](https://hexdocs.pm/elixir/NaiveDateTime.html) 등의 모듈을 참고할 수 있습니다.

## 또한 확인해보세요:
- [Date module](https://hexdocs.pm/elixir/Date.html) - Elixir에서 날짜 관련 작업을 위해 제공하는 모듈의 공식 문서입니다.
- [Calendar module](https://hexdocs.pm/elixir/Calendar.html) - Elixir에서 달력과 관련된 작업을 위해 제공하는 모듈의 공식 문서입니다.
- [DateTime module](https://hexdocs.pm/elixir/DateTime.html) - 날짜와 시간을 동시에 다룰 수 있는 Elixir의 모듈입니다.