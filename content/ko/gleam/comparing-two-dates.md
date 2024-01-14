---
title:                "Gleam: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜

우리는 일상 생활에서 종종 두 가지 날짜를 비교해야 할 때가 있습니다. 예를 들어, 어떤 사건이 발생한 시간을 계산하거나 마감일을 확인하기 위해선 두 날짜 간의 차이를 알아야 할 수도 있습니다. 이러한 상황에서 Gleam 프로그래밍 언어를 사용하여 두 날짜를 비교하는 방법에 대해 알아보겠습니다.

# 어떻게

먼저, ```gleam/datetime``` 모듈을 임포트해야 합니다. 그런 다음, 비교하려는 두 날짜를 각각 ```datetime.Datetime``` 구조체에 저장합니다. 예를 들어, 2021년 7월 1일과 2021년 6월 1일을 비교하려면 다음과 같이 코드를 작성할 수 있습니다.

```gleam
import gleam/datetime

let before = datetime.make(2021, 7, 1)
let after = datetime.make(2021, 6, 1)
```

이제 ```before```와 ```after```의 차이를 계산하여 일(day) 단위로 출력하려면, ```before - after``` 식을 사용하면 됩니다.

```gleam
let days = before - after
```

이렇게 하면 ```days``` 변수에 값으로 일(day) 단위의 차이가 저장됩니다. 따라서 출력 결과는 ```30```이 될 것입니다. Gleam은 날짜 비교를 위해 추가적인 라이브러리나 기능을 제공하지 않기 때문에 간단하면서도 효율적인 비교가 가능합니다.

# 깊이 파고들기

Gleam의 날짜와 시간 모듈은 Erlang 라이브러리를 기반으로 구현되기 때문에 ```erlang/calendar``` 모듈과 동일한 메소드와 구조를 공유합니다. 따라서 Gleam에서도 Erlang으로 날짜와 시간을 다루듯이 다룰 수 있습니다.

Gleam에서는 또한 ```gleam/datetime``` 모듈의 빠른 비교를 위해 엑셀 1900날짜 기준을 사용하므로 정확한 비교가 필요한 경우에는 Erlang 라이브러리를 직접 사용하는 것이 더 좋은 선택일 수 있습니다. 또한 Gleam이 Erlang과 같은 불변성(immutability)을 유지하기 때문에 날짜와 시간을 다루는 과정에서 발생할 수 있는 버그를 방지할 수 있습니다.

# 관련 링크

- [공식 Gleam 날짜 및 시간 문서](https://gleam.run/modules/gleam/datetime.html)
- [Erlang 달력 문서](https://erlang.org/doc/man/calendar.html)
- [Erlang에서 날짜 비교하기](https://9pmyohjewdum.medium.com/erlang-how-to-compare-dates-d1222b58c61c)

# 참고

이 글은 Gleam 공식 문서에서도 언급되는 내용을 바탕으로 작성되었습니다. 날짜와 시간을 다루는 기능은 Gleam과 Erlang 모두에서 동일하게 작동하기 때문에 해당 언어의 문서를 함께 참고하는 것을 추천합니다.