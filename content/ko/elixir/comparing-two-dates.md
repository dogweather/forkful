---
title:                "두 날짜 비교하기"
html_title:           "Elixir: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 날짜를 비교하는 것에 관심을 가질까요? 날짜 및 시간은 프로그래밍에서 중요한 역할을 합니다. 여러분이 기준 시간과 비교해서 로그를 정리하거나, 예약된 이벤트를 실행하거나, 특정 시간 이후에만 특정 작업을 수행하는 등 다양한 상황에서 날짜를 비교해야 할 수 있습니다.

## 어떻게
```Elixir
date1 = ~D[2021-06-01]
date2 = ~D[2021-05-15]

time1 = ~T[09:00:00]
time2 = ~T[12:00:00]

# 날짜 비교
date1 > date2 # true
date1 == date2 # false

# 시간 비교
time1 < time2 # true
time1 == time2 # false

# 날짜 및 시간 비교
DateTime.compare(date1, date2) # :gt (더 큰 값)
DateTime.compare(time1, time2) # :lt (더 작은 값)
DateTime.compare(date1, date2) # :eq (같은 값)

# 날짜 및 시간 차이 계산
Calendar.Chronos.diff(date1, date2, :days) # -17 (날짜 차이)
Calendar.Chronos.diff(time1, time2, :hours) # -3 (시간 차이)
```

## 깊이 파고들기
Elixir에서는 `Date`, `DateTime`, `Time` 모듈을 사용하여 날짜 및 시간을 다루는 것이 가능합니다. 이러한 모듈들은 Elixir에서는 단순한 유니 코드 코드 포인트를 사용하여 날짜 및 시간을 표현합니다. 하지만 이 모듈들을 사용하면 유효하지 않은 날짜 또는 시간을 만들어낼 수 있으니 주의해야 합니다. 유효하지 않은 날짜 또는 시간을 만들지 않기 위해서는 `Date` 모듈에 있는 `valid?` 함수를 사용하면 됩니다. 또한 `Calendar.Chronos` 모듈을 사용하면 날짜 및 시간의 차이를 계산할 수 있습니다.

## 더 알아보기
This is a Korean translation of article titled ["Comparing Dates in Elixir"](https://blog.appsignal.com/2020/07/22/comparing-dates-in-elixir.html) by AppSignal.