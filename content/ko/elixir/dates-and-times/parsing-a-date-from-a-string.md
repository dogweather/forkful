---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 02:05:42.717732-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\
  \uB294 \uAC83\uC740 \"2023-04-05\"\uC640 \uAC19\uC740 \uD14D\uC2A4\uD2B8\uB97C \uCDE8\
  \uD558\uC5EC \uD504\uB85C\uADF8\uB7A8\uC774 \uC774\uD574\uD558\uACE0 \uC791\uC5C5\
  \uD560 \uC218 \uC788\uB294 \uB0A0\uC9DC \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC774 \uC774 \uC791\uC5C5\uC744 \uD558\uB294 \uC774\uC720\uB294 \uB0A0\uC9DC\uAC00\
  \ \uB2E4\uC591\uD55C \uD615\uC2DD\uC73C\uB85C \uC874\uC7AC\uD558\uBA70, \uC774\uB97C\
  \ \uBE44\uAD50, \uC815\uB82C \uB610\uB294 \uC801\uC808\uD788 \uC800\uC7A5\uD558\uAE30\
  \ \uC704\uD574\uC11C\uB294\u2026"
lastmod: '2024-03-13T22:44:54.735733-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294\
  \ \uAC83\uC740 \"2023-04-05\"\uC640 \uAC19\uC740 \uD14D\uC2A4\uD2B8\uB97C \uCDE8\
  \uD558\uC5EC \uD504\uB85C\uADF8\uB7A8\uC774 \uC774\uD574\uD558\uACE0 \uC791\uC5C5\
  \uD560 \uC218 \uC788\uB294 \uB0A0\uC9DC \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uD30C\uC2F1\uD558\uAE30"
weight: 30
---

## 무엇 & 왜?

문자열에서 날짜를 파싱하는 것은 "2023-04-05"와 같은 텍스트를 취하여 프로그램이 이해하고 작업할 수 있는 날짜 형식으로 변환하는 것을 말합니다. 프로그래머들이 이 작업을 하는 이유는 날짜가 다양한 형식으로 존재하며, 이를 비교, 정렬 또는 적절히 저장하기 위해서는 일관성이 필요하기 때문입니다.

## 방법:

Elixir에서는 `Date` 모듈을 사용하여 날짜를 파싱할 수 있습니다. 문자열을 날짜로 변환하는 방법은 다음과 같습니다:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

예제 출력:

```elixir
~D[2023-04-05]
```

다른 형식을 처리하려면 `Timex` 라이브러리를 사용할 수 있습니다:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

예제 출력:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## 심층 분석

`Date.from_iso8601/1` 함수는 Elixir 표준 라이브러리의 일부로, ISO8601 날짜 표준(흔히 사용되는 날짜 형식)을 쉽게 파싱하기 위해 도입되었습니다. 하지만 실제 생활은 그렇게 간단하지 않으며; 날짜는 수많은 형식으로 존재합니다. 이때, 타사 Elixir 라이브러리인 `Timex`가 활용됩니다. `Timex`는 내장 Elixir 날짜 함수보다 풍부하며 다양한 날짜 형식을 처리하는 데 도움을 줍니다.

Elixir 자체는 불변성을 가지고 있으며, 이는 파싱된 날짜도 예외가 아니라는 것을 의미합니다; 생성된 후 변경될 수 없습니다. 이 특징은 Elixir의 함수형 프로그래밍 뿌리로 거슬러 올라가며, 예측 가능성 및 디버깅이 용이함을 보장합니다.

역사적으로, 다양한 표준으로 인해 날짜 파싱이 어려웠습니다. 그러나 `Timex`와 같은 라이브러리 및 Elixir의 언어 기능으로 복잡성이 추상화되어, 개발자의 삶을 조금 더 간단하게 만듭니다.

## 참조

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Timex 문서](https://hexdocs.pm/timex/Timex.html)
- [ISO8601 표준](https://www.iso.org/iso-8601-date-and-time-format.html)
