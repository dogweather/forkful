---
title:    "Gleam: 두 날짜 비교하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 왜

컴퓨터 코딩을 하는 사람들에게 날짜를 비교하는 것이 왜 필요한지 궁금할 수 있습니다. 날짜를 비교하는 것은 시간을 추적하고 분석하는 데 매우 유용합니다. 예를 들어, 어떤 일이 발생한 날짜와 비즈니스 활동의 성장을 분석하는 데 사용된 날짜를 비교할 수 있습니다.

# 어떻게

날짜를 비교하는 가장 쉬운 방법은 Gleam의 `DateTime` 모듈을 사용하는 것입니다. 먼저 두 날짜를 비교할 때 연도, 월, 일 등의 모든 요소가 일치하는지 확인해야 합니다. 그리고 `is_before` 또는 `is_after`를 사용하여 두 날짜 중 어느 것이 더 빠른지 확인할 수 있습니다. 아래의 코드 예제는 2020년 8월 14일을 나타내는 `my_date`와 오늘 날짜를 비교하는 예제입니다.

```Gleam
import gleam/datetime

my_date = DateTime.create(2020, 8, 14)
today = DateTime.now()

if (DateTime.is_before(my_date, today)) {
  println("my_date는 오늘보다 과거에 있습니다.")
} else if (DateTime.is_after(my_date, today)) {
  println("my_date는 오늘보다 미래에 있습니다.")
} else {
  println("my_date와 오늘은 같은 날짜입니다.")
}
```

위의 코드를 실행하면 오늘과 비교되는 `my_date`가 어느 쪽에 위치하는지 알 수 있습니다.

# 깊게 들어가기

날짜 비교는 `DateTime` 모듈의 여러 함수를 잘 활용하는 것이 중요합니다. 이 모듈을 사용하면 두 날짜를 비교하는 것뿐만 아니라 날짜 사이의 차이를 계산하거나, 특정 날짜의 다음 날짜를 계산하는 등 다양한 작업을 수행할 수 있습니다.

또한 `DateTime` 모듈은 국제 날짜와 시간을 처리할 수 있는 기능도 제공합니다. `DateTime.zone` 함수를 사용하여 특정 시간대를 설정하고, `DateTime.shift` 함수를 사용하여 다른 시간대로 변환할 수 있습니다.

더 많은 정보는 [Gleam 공식 문서](https://gleam.run/language/modules/datetime.html)를 참고하세요.

# 참고

- [Gleam 공식 사이트](https://gleam.run/)
- [Gleam 공식 문서](https://gleam.run/language/index.html)
- [Gleam 레퍼런스 문서](https://gleam.run/reference/index.html)