---
title:    "Gleam: 미래 또는 과거의 날짜 계산"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜?

일정을 계산하면서 년, 월, 일이 나타난 날짜를 찾는 것은 일상적인 일입니다. 그러나 지난 날짜나 미래의 날짜 계산은 도대체 어떤 이유 때문에 필요할까요? 수업 시간표, 여행 계획, 사업 계획 등 다양한 상황에서 과거나 미래의 날짜를 계산하고 싶을 수 있습니다.

# 어떻게

우리는 Gleam에서 간단한 코드를 작성하여 날짜 계산 기능을 구현할 수 있습니다. 먼저 `date` 모듈을 불러오고, `date_diff` 함수를 사용하여 과거나 미래의 날짜와 현재 날짜 사이의 일수 차이를 계산합니다. 그리고 `add` 함수를 사용하여 계산한 일수만큼 더하거나 빼 줌으로써 원하는 날짜를 얻을 수 있습니다. 아래는 예시 코드와 출력 결과입니다.

```Gleam
import date

let past_date = date.add(date.now(), date_diff(-7))
let future_date = date.add(date.now(), date_diff(14))

IO.print("7 days ago: " ++ past_date)
IO.print("14 days from now: " ++ future_date)
```

**출력 결과:**

```
7 days ago: 2021-11-15T22:12:33Z
14 days from now: 2021-12-06T22:12:33Z
```

# 깊게 들어가기

더 복잡한 날짜 계산을 할 때는 `date.add` 함수 대신 `add_duration` 함수를 사용해도 됩니다. 이 함수는 `Duration` 타입의 값을 인자로 받아 계산된 날짜를 반환합니다. 또한 `date.compare` 함수를 사용하면 두 날짜를 비교하여 어느 날짜가 더 빠른지 혹은 더 늦은지 판단할 수 있습니다.

또한 `date.Format` 모듈을 사용하면 날짜를 원하는 형식으로 출력할 수 있습니다. `date.Fromat.to_string` 함수를 사용하여 날짜와 특정 형식을 인자로 넘겨주면 원하는 포맷의 날짜 문자열을 얻을 수 있습니다.

# 나아가서

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Gleam 날짜 관련 모듈](https://gleam.run/documentation/stdlib/date/)
- [공식 Gleam 커뮤니티](https://gleam.run/community/)