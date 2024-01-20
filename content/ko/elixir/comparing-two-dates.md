---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

두 날짜를 비교하는 것은 일련의 날짜들 중 특정 날짜가 더 이전이거나 이후인지를 확인하는 프로그래밍 행위입니다. 프로그래머들은 앱의 로직이나 데이터 분석과 같은 다양한 문제를 해결하기 위해 이를 사용합니다.

## 사용법:

Elixir의 "Date.compare" 함수를 이용해 두 날짜를 비교할 수 있습니다.

```Elixir
date1 = Date.from_iso8601!("2022-01-01") 
date2 = Date.from_iso8601!("2023-01-01")

IO.inspect(Date.compare(date1, date2))  
// Sample output: {:lt, %{calendar: Calendar.ISO, day: 1, month: 1, year: 2022}}
```

위의 코드에서 `{:lt, %{calendar: Calendar.ISO, day: 1, month: 1, year: 2022}}` 출력은 첫 번째 날짜가 두 번째 날짜보다 빠르다는 것을 의미합니다.

## 깊이 가보기:

두 날짜를 비교하는 작업은 데이터베이스 조회, 빌링 주기 관리 등의 용도로 여러 분야에서 이용됩니다. Elixir에서는 'Date.compare' 함수를 제공하여 서로 다른 두 날짜를 API에서 비교하는 것을 간단하게 해 줍니다. 다른 비교 방법으로는 각 날짜를 Unix 타임스탬프로 변환하고 그 값을 비교하는 방법도 있습니다.

## 참고 자료:

- Elixir 공식 문서의 날짜 비교 섹션 : https://hexdocs.pm/elixir/Date.html#compare/2 
- 날짜를 Unix 타임스탬프로 변환: https://hexdocs.pm/elixir/DateTime.html#to_unix/2