---
title:                "날짜를 문자열로 변환하기"
html_title:           "Elixir: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 프로그래머가 날짜와 시간 정보를 더 직관적이고 사용하기 쉬운 형식으로 바꾸기 위한 일입니다. 이를 통해 더 쉽게 날짜와 시간을 표현하고 관리할 수 있습니다.

## 하는 방법:

```Elixir
# Elixir에서 현재 날짜를 문자열로 변환하는 방법:

date = Date.utc_today()  # 현재 날짜 가져오기
output = Date.to_string(date)  # 날짜를 문자열로 변환
IO.inspect(output)  # 결과 출력
```

출력:
```
"2021-11-23"
```

```Elixir
# Elixir에서 특정 날짜를 원하는 형식에 맞게 문자열로 변환하는 방법:

date = Date.new(2021, 11, 23)  # 원하는 날짜 정보 입력
format = "{0}/{1}/{2}"  # 원하는 형식 지정
output = Date.to_string(date, format)  # 날짜를 문자열로 변환
IO.inspect(output)  # 결과 출력
```

출력:
```
"11/23/2021"
```

## 깊이 파헤치기:

(1) 이 기능은 Elixir의 날짜 라이브러리인 `Date` 모듈에서 제공됩니다. 이러한 기능은 프로그래밍 언어나 라이브러리에서 일반적으로 가지는 기능 중 하나로, 날짜와 시간 정보를 더 쉽게 다룰 수 있도록 합니다.

(2) 날짜를 문자열로 변환하는 다른 방법으로는 `DateTime` 모듈의 `to_string/2` 함수를 사용하는 방법이 있습니다. 이 방법의 경우 시간 정보까지 포함되어 더 상세하게 문자열로 변환할 수 있습니다.

(3) `to_string/2` 함수의 내부 구현은 `__to_string__/2` 함수를 호출하여 해당 데이터를 문자열로 변환하는 것입니다. 이를 통해 사용자 정의 데이터 타입에서도 `to_string/1` 함수를 통해 문자열로 변환할 수 있습니다.

## 더 알아보기:

- [Date 모듈 문서](https://hexdocs.pm/elixir/Date.html)
- [DateTime 모듈 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir에서 날짜와 시간 다루기](https://elixir-lang.org/getting-started/datetime.html)