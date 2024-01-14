---
title:    "Elixir: 현재 날짜 가져오기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

일자를 얻는 것에 참여하는 이유는 무엇일까요? 일자는 프로그래밍에서 중요한 요소이며, Elixir에서 현재 일자를 얻는 것은 간단하고 편리합니다.

# 어떻게

다음은 현재 일자를 얻는 방법에 대한 주요 코드 예제입니다.
```Elixir
{:ok, date} = Date.utc_today()
IO.puts(date)
```

출력:
```
2021-01-13
```

범위를 지정한 다음 일자를 얻는 방법도 있습니다.
```Elixir
{:ok, date} = Date.utc_today(2021, 12, 25)
IO.puts(date)
```

출력:
```
2021-12-25
```

또한 시간대를 변경하거나, 특정 지역의 일자를 얻는 방법도 있습니다. 더 많은 정보는 공식 Elixir 문서를 참고해주세요. 

# 깊은 곳에서

Elixir에서 현재 일자를 얻는 방법은 내부적으로 Erlang의 날짜와 시간 라이브러리인 `calendar`를 사용합니다. 이는 시간대, 윤초, 그리고 다양한 형식의 일자를 다룰 수 있는 강력한 기능을 제공합니다.

# 참고

- [공식 Elixir 문서](https://hexdocs.pm/elixir/Date.html)
- [Elixir Date 라이브러리 소스 코드](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/date.ex)