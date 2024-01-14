---
title:                "Gleam: 현재 날짜 가져오기"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

왜 현재 날짜를 가져오는 것이 중요한지 궁금하신가요? 일상 생활에서 사용되는 많은 프로그램들은 현재 날짜를 표시하는데 사용됩니다. 예를 들어, 예약 시스템이나 일정 관리 프로그램, 블로그 게시물의 작성 날짜 표시 등이 그 예입니다. 따라서 현재 날짜를 정확하게 가져오는 것은 프로그램의 정확성을 보장하는 데 매우 중요합니다.

## 하는 법

Gleam에서는 쉽게 현재 날짜를 가져올 수 있습니다. 아래의 코드를 참고하시면 됩니다.

```Gleam
import gleam/calendar

let today = calendar.now()
```

위의 코드에서는 `gleam/calendar` 모듈을 불러와 `now()` 함수를 호출하여 현재 날짜를 가져옵니다. 가져온 날짜는 `today` 변수에 할당되어 사용할 수 있게 됩니다. 아래는 예제 출력 결과입니다.

```Gleam
2021-09-25
```

위의 예시 코드에서는 기본적으로 날짜의 연도, 월, 일 정보만을 가져옵니다. 만약 더 자세한 정보, 예를 들어 요일 정보 등을 함께 가져오고 싶다면 `now()` 함수의 인자로 `gleam/calendar` 모듈에서 제공하는 `Options` 타입의 값을 전달해주면 됩니다. 아래는 요일 정보를 함께 가져오는 예시 코드입니다.

```Gleam
import gleam/calendar

let options = calendar.Options(
  weekday: true
)

let today = calendar.now(options)
```

## 심층 분석

`gleam/calendar` 모듈에서는 현재 날짜를 가져오는 `now()` 함수 외에도 다양한 함수들을 제공합니다. 예를 들어 `add()` 함수는 날짜에 일정 기간을 더하는 등의 기능을 제공하며, `compare()` 함수는 두 날짜를 비교하는 등의 기능을 제공합니다. 이러한 함수들을 적절히 사용하면 다양한 날짜 기능을 구현할 수 있습니다.

## 참고 자료

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Gleam을 사용해 현재 날짜 출력하기](https://gleam.run/documentation/tutorials/getting-the-current-date/)
- [Gleam Calendar 모듈 문서](https://gleam.run/module/gleam/calendar/latest/)