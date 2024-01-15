---
title:                "현재 날짜 가져오기"
html_title:           "Gleam: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 얻는 방법에 대해 궁금해 졌다면, 당신은 고민할 필요가 없습니다! Gleam에서 제공하는 내장 함수를 사용하여 쉽게 현재 날짜를 얻을 수 있습니다.

## 사용 방법

``` Gleam
import gleam/time

let today = time.now()

// 2021-05-27T14:18:06+00:00
```

위의 코드는 현재 날짜와 시간을 UTC로 출력합니다. 만약 자신의 타임존에 맞게 출력하고 싶다면, `now()` 함수를 `now_tz()`로 바꾸어 사용할 수 있습니다.

``` Gleam
let today = time.now_tz("Asia/Seoul")

// 2021-05-27T23:18:06+09:00
```

또한, `local_now()` 함수를 사용하여 현재 로컬 시간을 얻을 수도 있습니다.

``` Gleam
let now = time.local_now()

// 2021-05-27T23:18:06+09:00
```

더 많은 사용 예제를 보려면 [Gleam 공식 문서](https://gleam.run/documentation/standard-library/#dates-and-time)를 참고해주세요.

## 더 들어가기

Gleam에서는 내장 함수를 통해 현재 날짜를 얻는 것 외에도 다양한 작업을 할 수 있습니다. 예를 들어, `add()` 함수를 사용하면 날짜에 일정 기간만큼 더하거나 빼는 것도 가능합니다. 또한, `format()` 함수를 사용하여 날짜를 원하는 형식으로 표현할 수 있습니다.

더 많은 내장 함수와 예제는 [Gleam 공식 문서](https://gleam.run/documentation/standard-library/#dates-and-time)에서 확인할 수 있습니다.

## 더 알아보기

현재 날짜를 얻는 것은 프로그램에서 매우 중요한 작업입니다. Gleam에서는 일반적으로 사용되는 구문을 내장 함수로 제공하므로 더 간단하게 현재 날짜를 다룰 수 있습니다. 하지만, 이 내장 함수들이 어떻게 동작하는지 궁금하다면 [Gleam의 소스 코드](https://github.com/gleam-lang/gleam/blob/master/lib/std/src/time.gleam)를 살펴보는 것도 좋은 학습 방법입니다.

## 관련 링크

[Gleam 날짜와 시간 관련 공식 문서](https://gleam.run/documentation/standard-library/#dates-and-time)

[Gleam 소스 코드에서 날짜와 시간 관련 내장 함수 확인하기](https://github.com/gleam-lang/gleam/blob/master/lib/std/src/time.gleam)