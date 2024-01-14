---
title:                "Elixir: 날짜를 문자열로 변환하기"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 왜 필요한지 궁금하십니까? 당신은 날짜를 데이터베이스에 저장하거나 사용자에게 표시하기 위해 문자열로 변환해야 할 때가 있습니다. 또한 날짜를 형식화하여 원하는 형식으로 표시해야 할 수도 있습니다.

## 하는 방법

```Elixir
# Date 데이터를 문자열로 변환하는 코드 예시
date = ~D[2020-05-15] # 년-월-일 형식으로 날짜를 설정
string = to_string(date) # 문자열로 변환
IO.puts(string) # 결과: "2020-05-15"
```

또한 Elixir는 다양한 형식의 날짜를 문자열로 변환하는 다양한 함수를 제공합니다. 예를 들어, `~D[2020-05-15]`대신 `~D[15-05-2020]`와 같이 다른 형식으로 날짜를 설정할 수 있습니다. 또한 날짜에 시간 정보를 추가하고 싶다면 `~N[2020-05-15 12:30:00]`와 같이 사용자 지정 시간 정보를 추가하여 날짜를 설정할 수도 있습니다.

## 심층 연구

Elixir에서 날짜를 문자열로 변환하는 데 사용하는 함수는 `to_string/1`입니다. 이 함수는 모든 타입의 데이터를 문자열로 변환하는 데 사용됩니다. 그러나 날짜를 형식화하는 데 사용되는 가장 일반적인 함수는 `~D` 형식 요소와 `to_string/2` 함수입니다.

또한 Elixir에서는 `~U` 형식 요소를 사용하여 특정 시간 정보가 포함된 문자열을 파싱하여 날짜로 변환하는 것도 가능합니다. 예를 들어, `~U[2020-05-15 12:30:00]`와 같이 사용자가 입력한 시간 정보가 포함된 문자열을 지정한 날짜로 파싱할 수 있습니다.

## 더 알아보기

- [DateTime 모듈 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir 날짜 형식 지정자 문서](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#~D/1)
- [Elixir 문자열 파싱 문서](https://hexdocs.pm/elixir/String.html#module-parsing)