---
title:                "Gleam: 두 날짜 비교하기"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 개의 날짜를 비교하는 것이 왜 중요한지 궁금하신가요? 그렇다면 이 블로그 포스트를 읽어보세요! 두 날짜를 비교하는 방법을 알려주고, 왜 이를 활용하는 지에 대해 소개해 드릴 것입니다.

## 어떻게
두 개의 날짜를 비교하는 방법은 매우 간단합니다. 먼저, 두 날짜를 각각 변수에 할당합니다. 그 다음 "```Gleam
```" 코드 블록 안에 `gleam/date` 에서 제공하는 `compare` 함수를 사용하여 두 날짜를 비교해보세요. 예를 들면 아래와 같습니다.

```
Gleam
import gleam/date

let today = date.now()
let yesterday = date.add_days(today, -1)

date.compare(today, yesterday)
```
위 코드는 오늘 날짜와 어제 날짜를 비교하고 있습니다. `compare` 함수는 첫 번째 매개변수가 두 번째 매개변수보다 크면 `Greater`를, 작으면 `Less`를, 같으면 `Equal`을 반환합니다. 코드를 실행해보면 `Less`가 출력될 것입니다.

## 깊게 파보기
두 개의 날짜를 비교할 때는 더 많은 정보가 필요할 수도 있습니다. 예를 들어 두 날짜 사이의 차이를 구하고 싶다면 어떻게 해야할까요? 이를 위해서는 `compare` 함수를 사용한 다음 `diff` 함수를 사용합니다.

```
Gleam
import gleam/date

let today = date.now()
let yesterday = date.add_days(today, -1)

date.diff(today, yesterday)
```
위 코드는 오늘 날짜와 어제 날짜 사이의 차이를 일수로 반환합니다. 만약 날짜 사이의 차이를 시간 단위로 구하고 싶다면 `diff` 함수의 두 번째 매개변수에 `Time` 타입을 추가해주면 됩니다.

## 더 읽어보기
더 많은 정보를 원하신다면 아래 링크를 참고해보세요.

- [Gleam 공식 문서](https://gleam.run/)
- [Gleam Date 라이브러리 문서](https://gleam.run/modules/date)
- [Gleam 공식 Github 레포지토리](https://github.com/gleam-lang/gleam)