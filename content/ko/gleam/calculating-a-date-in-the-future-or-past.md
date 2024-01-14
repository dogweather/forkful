---
title:                "Gleam: 미래 또는 과거 날짜 계산하기"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

날짜를 미래나 과거로 계산하는 것이 어떻게 유용한지 궁금하신가요? 일정한 날짜나 기간에 대한 정보를 얻기 위해서는, 현재 날짜를 기준으로 미래나 과거의 날짜를 계산하는 것이 필요합니다. 예를 들어, 네 회사에서 일하기 위해 며칠의 휴가를 써야 할지 계산해야 하는 상황이라면, 미래의 날짜를 계산하는 작업이 필요합니다.

# 하우 투

```Gleam
import Gleam.Date

let future_date = Date.add_months(2021-08-01, 3)
let past_date = Date.add_years(2021-08-01, -5)

IO.println("3개월 후의 날짜는 {future_date}입니다.")
IO.println("5년 전의 날짜는 {past_date}입니다.")
```

위의 코드 예시에서는, 현재 날짜를 기준으로 미래와 과거의 날짜를 각각 3개월 뒤와 5년 전으로 계산한 후 출력하는 예시를 보여줍니다. Gleam의 Date 모듈을 이용하면, 다양한 날짜 계산 함수를 사용할 수 있습니다. 미래나 과거의 날짜를 계산할 때는 해당 날짜를 기준으로 얼마만큼의 일, 개월, 혹은 연도를 더하거나 빼면 됩니다.

# 딥 다이브

날짜를 계산하는 것은 알고리즘과 테크닉의 조합으로 이루어져 있습니다. Gleam에서 제공하는 Date 모듈은 우리가 말하는 "날짜"를 어떻게 표현하고 계산하는지에 대해서도 알아봐야 합니다. 일반적으로 우리는 년, 월, 일의 조합으로 날짜를 나타냅니다. 하지만 이는 단순한 숫자로만 이루어져 있기 때문에, 다른 시간대나 의미를 갖고 있는 날짜와 구분하기 쉽지 않습니다. 따라서 Gleam에서는 Date 레코드 타입을 사용하여 날짜를 나타내는데, 이는 년, 월, 일 외에도 시, 분, 초 등 다양한 정보를 담을 수 있도록 구조화 되어 있습니다.

# 참고

- [Gleam의 Date 모듈 문서](https://gleam.run/core-libraries/date.html)
- [ISO 8601 날짜 표현 방식에 대한 정보](https://ko.wikipedia.org/wiki/ISO_8601)
- [더 많은 Gleam 예제 및 자료](https://github.com/gleam-lang/awesome-gleam)