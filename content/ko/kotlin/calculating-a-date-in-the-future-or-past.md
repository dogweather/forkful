---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Kotlin: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

날짜를 미래나 과거로 계산하는 것은 매우 유용한 프로그래밍 기술입니다. 예를 들어, 사용자가 특정 날짜까지만 유효한 계약을 체결하도록 하거나, 특정 날짜에 이벤트를 알림으로 설정하는 등 여러 상황에서 사용할 수 있습니다. 프로그래머들은 이러한 날짜 계산 기술을 사용하여 프로그램의 유용성을 높이고 효율적인 코드를 작성할 수 있습니다.

## 어떻게?

Kotlin은 날짜 계산을 쉽게 할 수 있도록 다양한 함수를 제공합니다. 먼저, 현재 날짜를 가져와서 새로운 날짜를 계산하는 방법을 알아봅시다. 예를 들어, 현재 날짜에서 7일 뒤의 날짜를 계산하고 싶다면 ```val futureDate = LocalDate.now().plusDays(7)```와 같이 작성하면 됩니다. 또한, 이전 날짜를 계산하는 방법도 비슷합니다. 현재 날짜에서 7일 전의 날짜를 계산하고 싶다면 ```val pastDate = LocalDate.now().minusDays(7)```와 같이 작성하면 됩니다. 이처럼 Kotlin의 날짜 계산 함수를 사용하면 매우 간단하게 날짜를 계산할 수 있습니다.

## 깊이 파헤치기

날짜 계산 기술은 오래 전부터 사용되어왔습니다. 예를 들어, 로마 시대에는 매달 31일까지 있는 달력을 사용했습니다. 하지만 현재는 더 정확한 달력이 사용되고 있습니다. 이외에도 날짜를 계산하기 위해 여러 가지 다른 방법들이 존재합니다. 예를 들어, calendar 클래스를 사용하는 방법이 있으며, 자바 언어에서도 사용할 수 있는 일반적인 방법이기도 합니다. 하지만 Kotlin에서 제공하는 날짜 계산 함수를 사용하는 것이 가장 간단하고 효율적인 방법입니다.

## 관련 링크

- [Kotlin 공식 문서: 날짜 계산](https://kotlinlang.org/docs/reference/dates.html)
- [Kotlin 날짜 함수 예제](https://www.programiz.com/kotlin-programming/examples/dates-current-next)
- [블로그 포스트: Kotlin을 이용한 날짜 계산 방법](https://medium.com/@damesavram/working-with-dates-in-kotlin-957b9c651da6)