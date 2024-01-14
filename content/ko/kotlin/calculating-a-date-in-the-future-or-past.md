---
title:    "Kotlin: 미래나 과거의 날짜 계산하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜
날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 단지 1-2 문장으로 설명합니다.

일이 언제 일어날 지 아는 것은 매우 중요합니다. 예를 들어, 우리는 친구의 생일이나 중요한 기념일을 잊지 않기 위해 미리 계산을 해야 할 수 있습니다. 또한 특정 일정을 설정하거나 스케줄 관리에 도움을 주기 위해 미래의 날짜를 사용할 수도 있습니다.

날짜를 미래나 과거로 계산하는 것은 매우 쉽습니다. 코틀린 코드에서 날짜를 계산하는 방법에 대해 알아보겠습니다.

## 해하는 방법
코틀린에서 날짜를 계산하는 방법은 매우 간단합니다. 우선, 우리가 계산할 날짜를 생성해야 합니다. 이를 위해서는 DateTimeFormatter를 사용해 원하는 형식의 문자열로 날짜를 생성합니다.

```kotlin
// 현재 날짜와 시간 생성
val now = LocalDateTime.now()
// 10년 후의 날짜 생성
val nextTenYears = now.plusYears(10)
// 50일 전의 날짜 생성
val lastFiftyDays = now.minusDays(50)
```

위 코드에서 우리는 `LocalDateTime` 클래스의 `now()` 메서드를 사용하여 현재 날짜와 시간을 생성하고, `plusYears()` 및 `minusDays()` 메서드를 사용하여 미래나 과거의 날짜를 계산하는 방법을 볼 수 있습니다. 이렇게 계산된 날짜는 모두 `LocalDateTime` 클래스의 인스턴스로 반환됩니다.

또 다른 예시로, 두 날짜 사이의 일 수를 계산하는 방법도 알아보겠습니다.

```kotlin
// 미래 날짜 생성
val futureDate = LocalDateTime.of(2022, 9, 1, 0, 0)
// 현재 날짜와 비교하여 남은 일수 계산
val daysLeft = Duration.between(now, futureDate).toDays()
println("남은 일 수: $daysLeft 일")
```

위 코드에서는 `LocalDateTime` 클래스의 `of()` 메서드를 사용하여 특정 미래의 날짜를 생성하고, `Duration` 클래스의 `between()` 메서드를 사용하여 현재 날짜와 비교하여 남은 일 수를 계산하는 방법을 볼 수 있습니다.

## 딥 다이브
날짜를 미래나 과거로 계산하는 방법에 대해 더 깊이 알아보겠습니다.

이전에 살펴본 코드에서 우리는 `plusYears()`, `minusDays()` 등의 메서드를 사용하여 날짜를 계산했습니다. 이 외에도 `plusMonths()`, `minusHours()` 등 다양한 메서드를 사용하여 원하는 시간 단위로 날짜를 계산할 수 있습니다.

또한, 날짜를 비교하는 데에도 많이 사용되는 `compareTo()` 메서드가 있습니다. 이 메서드는 날짜를 숫자로 나타내어 두 날짜를 비교할 수 있도록 합니다. 예를 들어, 두 날짜 중 어느 날짜가 더 앞선지를 비교하고 싶을 때 유용합니다.

이 외에도 `DateTimeFormatter` 클래스를 사용하여 날짜를 원하는 형식으로 출력할 수 있습니다. 예를 들어, 날짜를 "yyyy-MM-dd" 형식으로 출력하