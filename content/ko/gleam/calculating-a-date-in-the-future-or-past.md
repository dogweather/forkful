---
title:    "Gleam: 미래 또는 과거의 날짜 계산하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜?
날짜를 미래나 과거로 계산하는 것에 대해 왜 관심을 가질까요? 나중에 특정 날짜에 기념일이나 중요한 일정이 있을 수 있고, 그 날짜를 미리 계산하고 알아두는 것이 유용할 수 있습니다.

## 계산 방법
지금부터는 Gleam 프로그래밍 언어로 날짜를 계산하는 방법을 소개하겠습니다. 먼저 현재 날짜를 알아내는 함수를 사용하여 오늘의 날짜를 구하고, 이를 변수에 저장합니다. 그 후에 이 변수를 이용하여 미래나 과거의 날짜를 계산하고 출력할 수 있습니다.

```Gleam
import Time.Date

fn main() {
  let today = Date.today()
  let future_date = Date.add_days(today, 7)
  let past_date = Date.sub_days(today, 14)

  io.println("오늘: {}", today)
  io.println("일주일 후: {}", future_date)
  io.println("14일 전: {}", past_date)
}
```

위의 코드를 실행하면, 다음과 같은 출력을 볼 수 있습니다.

```
오늘: 2021-04-28
일주일 후: 2021-05-05
14일 전: 2021-04-14
```

## 깊이 들어가기
날짜를 계산하는 방법에 대해 깊이 있는 정보를 알고 싶다면, Gleam 공식 문서에서 Time.Date 모듈을 자세히 살펴보는 것을 추천합니다. 이 모듈은 다양한 날짜 계산 함수를 제공하고 있으며, 파라미터를 추가하여 더 복잡한 계산도 가능합니다. 또한 이 모듈을 사용하여 현재 시간과 날짜를 다루는 것도 가능합니다.

## 참고 자료
- [Gleam 공식 문서 - Time.Date 모듈](https://gleam.run/documentation/stdlib/time.date/)
- [Gleam 프로그래밍 언어 소개 (한국어)](https://gleam.run/documentation/getting-started-korean/)
- [Date 계산 관련 함수들 (영어)](https://gleam.run/playground/2014)

## 참고 자료
- [Gleam 공식 문서 - Time.Date 모듈](https://gleam.run/documentation/stdlib/time.date/)
- [Gleam 프로그래밍 언어 소개 (한국어)](https://gleam.run/documentation/getting-started-korean/)
- [Date 계산 관련 함수들 (영어)](https://gleam.run/playground/2014)