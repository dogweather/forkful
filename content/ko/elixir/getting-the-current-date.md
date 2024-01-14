---
title:    "Elixir: 현재 날짜 받아오기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 왜 날짜를 가져와야 하는가?

Elixir는 매우 다양한 언어이지만 강력한 날짜 관련 기능을 제공합니다. 따라서 현재 날짜를 얻는 것은 굉장히 유용할 수 있습니다. 예를 들어 데이터베이스의 일부와 연결된 어플리케이션을 만들 때 많은 사람들이 현재 날짜를 가져와서 패턴을 작성합니다. Elixir는 이 작업을 매우 쉽게 만들어줍니다.

# 방법

```elixir
# 현재 날짜를 가져오는 코드
current_date = Date.utc_today()

# 현재 날짜와 시간을 가져오는 코드
current_date_time = DateTime.utc_now()
```

두 가지 방법 모두 매우 간단합니다. `Date.utc_today()` 함수를 사용하면 현재 날짜만 가져올 수 있고, `DateTime.utc_now()` 함수를 사용하면 현재 날짜와 시간을 함께 가져올 수 있습니다.

# 자세히 알아보기

Elixir는 Erlang 라이브러리인 `Calendar`을 기반으로 날짜와 시간을 다룹니다. 따라서 `Date`와 `DateTime` 모듈을 사용해 일반적인 날짜 형식을 다룰 수 있습니다. 예를 들어 `Date.new(year, month, day)` 함수를 사용해 지정된 날짜를 만들 수 있습니다. 또한 `DateTime.new(year, month, day, hour, minute, second)` 함수를 사용해 지정된 시간을 가진 날짜를 만들 수 있습니다.

# 관련 자료

- [Elixir Date 모듈 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime 모듈 문서](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Calendar 모듈 문서](https://hexdocs.pm/elixir/Calendar.html)