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

## 왜 
다른 프로그래밍 언어에 비해 Elixir가 날짜를 문자열로 변환하는 것이 유용한 이유는 무엇일까요? 그 이유에 대해 간단하게 알아보도록 하겠습니다.

## 어떻게
Elixir에서 날짜를 문자열로 변환하는 방법에 대해 살펴보겠습니다. 먼저, `Date` 모듈의 `to_string` 함수를 사용할 수 있습니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다.

```Elixir
Date.to_string(Date.utc_today())
```

위의 코드를 실행하면 현재 날짜를 UTC 시간대로 변환한 후 문자열로 반환합니다. 예상되는 결과는 "2021-09-20"과 같습니다. 또 다른 방법으로는 `Calendar` 모듈의 `to_string` 함수를 사용하는 것입니다. 아래의 예제 코드를 살펴보세요.

```Elixir
Calendar.to_string(System.system_time(:millisecond))
```

위 코드는 현재 시간을 밀리초 단위로 반환한 다음 문자열로 변환합니다.

## 깊이 들어가보기
날짜를 문자열로 변환하는 더 깊은 내용에 대해 알아보겠습니다. Elixir에서는 날짜와 관련된 여러 가지 모듈을 제공합니다. 예를 들어, `Date`, `Calendar`, `Time`, `DateTime` 등이 있습니다. 각각의 모듈에는 날짜와 관련된 다양한 함수들이 있으며, 이를 조합해서 사용할 수 있습니다. 또한, 다양한 시간대를 지원하므로 원하는 시간대로 날짜를 변환할 수도 있습니다.

## 더 알아보기
자세한 내용을 학습하기 위해 아래의 링크들을 참고해보세요.

- Elixir 날짜 모듈 문서: https://hexdocs.pm/elixir/Date.html
- Elixir 캘린더 모듈 문서: https://hexdocs.pm/elixir/Calendar.html
- Elixir 시간 모듈 문서: https://hexdocs.pm/elixir/Time.html
- Elixir 날짜 및 시간 관련 함수 문서: https://hexdocs.pm/elixir/1.12/calendar-and-time-functions.html

## 참고 자료
- Elixir 공식 문서: https://elixir-lang.org/docs.html
- Elixir 커뮤니티: https://elixirforum.com/