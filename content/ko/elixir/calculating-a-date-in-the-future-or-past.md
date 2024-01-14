---
title:    "Elixir: 미래나 과거의 날짜를 계산하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

"# 왜
날짜를 미래나 과거로 계산하는 것에 대해 배우는 것은 중요합니다. Elixir에서 날짜를 계산하는 것은 매우 유용하며 더 나은 앱 개발을 위해 필수적입니다. 이 포스트에서는 어떻게 해야 하는지 알려줄 것입니다.

## 어떻게하기
```
Elixir.DateTime.utc_today
|> Elixir.Calendar.Date.add(10)
|> Elixir.DateTime.to_naive
```
이 코드는 현재 시간을 가져와서 10일을 더하는 예시입니다. 결과는 현재 시간에서 10일 후의 날짜가 됩니다. 이와 같이 Elixir에서 날짜를 계산할 수 있습니다.

## 심층 분석
날짜와 시간을 다뤄야 하는 애플리케이션에서는 과거와 미래의 날짜를 계산하는 기능이 필수적입니다. Elixir에서는 DateTime 및 Calendar 모듈을 사용하여 간단하게 날짜 및 시간을 계산할 수 있습니다. 대부분의 언어에서 날짜 및 시간을 다루는 것은 복잡하고 어려운 과정일 수 있지만, Elixir에서는 간단하고 직관적인 방법으로 처리할 수 있습니다.

# 관련 자료
- Elixir 날짜 및 시간 공식 문서 (https://hexdocs.pm/elixir/DateTime.html)
- Elixir에서 날짜와 시간 다루기 (https://thoughtbot.com/blog/elixir-date-and-time)
- Elixir와 날짜 형식 (https://devclass.com/2019/08/12/elixir-io-format-dates/)
- Elixir의 정교한 Date API (https://dockyard.com/blog/2018/07/06/a-polyglot-s-guide-to-elixir-part-2-date-time-operations)