---
title:    "Gleam: 현재 날짜 가져오기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 것에 대해 고민하고 계신가요? 그렇다면 당신은 코딩에서 시간과 날짜에 대한 정보를 사용하고 있을 가능성이 높습니다. 날짜는 많은 프로그래밍 작업에서 중요한 역할을 하며, 현재 날짜를 얻는 것은 매우 유용합니다.

## 어떻게

Gleam에서 현재 날짜를 가져오는 방법은 매우 쉽습니다. `Date.now()` 함수를 사용하면 됩니다. 아래의 코드 예제를 통해 확인해보세요.

```Gleam
let current_date = Date.now()
```

위의 코드를 실행하면 현재 날짜와 시간 정보가 포함된 `DateTime` 값이 반환됩니다. 이를 원하는 형식으로 변경하거나 다른 계산에 사용할 수 있습니다.

## 깊이 들어가기

현재 날짜를 가져오기 위해서는 Gleam 언어 내에서 `DateTime` 타입을 다루는 방법을 알아야 합니다. `DateTime` 값은 날짜와 시간을 포함하며, 올바른 형식으로 변환될 수 있도록 구조화된 값입니다. 이를 문자열로 변환하거나 다른 타입으로 변환하는 방법을 익히는 것이 중요합니다.

## 참고자료

See Also:

- Gleam 문서: https://gleam.run/documentation
- 날짜를 다루는 다른 방법: https://gleam.run/s/guides/dates
- Gleam 커뮤니티: https://gleam.run/community