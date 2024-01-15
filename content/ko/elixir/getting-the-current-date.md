---
title:                "현재 날짜 가져오기"
html_title:           "Elixir: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

"## 왜"
현재 날짜를 얻는 것에 대해 할 시간이 왜 필요한지에 대해 최대 2문장으로 설명합니다.

"## 어떻게"
"```Elixir
Date.utc_today()
```
를 사용하여 현재 날짜 및 시간을 얻을 수 있습니다.

"```Elixir
{:ok, {{year, month, day}, {hour, minute, second}}} = DateTime.utc_now("Etc/UTC")
IO.puts("오늘 날짜는 #{year}년 #{month}월 #{day}일이고, 현재 시각은 #{hour}시 #{minute}분 #{second}초입니다.")
```

위의 코드는 Elixir에서 현재 날짜와 시각을 얻는 방법을 보여줍니다. 첫 번째 예제에서는 `Date.utc_today()` 함수를 사용하여 UTC 표준에서 현재 날짜를 얻었고, 두 번째 예제에서는 `DateTime.utc_now()` 함수를 사용하여 시간대를 지정하여 현재 날짜와 시간을 얻었습니다. 특정 시간대를 지정하지 않는 경우, 내 컴퓨터의 현재 시간대를 기준으로 날짜와 시간을 얻게 됩니다.

"## 딥 다이브"
위 코드에서 `Date` 및 `DateTime` 모듈을 사용하여 현재 날짜와 시각을 얻는 과정을 살펴보았습니다. 이러한 모듈은 Elixir에서 날짜와 시간을 다루는 데 도움을 줍니다. 추가적으로, 특정한 시간대를 고려하여 날짜와 시간을 다루려면 `Timex` 라이브러리를 사용할 수도 있습니다.

"See Also"
- [Elixir Date 모듈 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime 모듈 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Timex 라이브러리 문서](https://hexdocs.pm/timex/readme.html)