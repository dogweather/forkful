---
title:                "Ruby: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
우리는 때때로 특정 일과 다른 일 사이의 차이점을 비교해야 할 때가 있습니다. 이 때 두 날짜를 비교하면 유용한 경우가 많습니다. 루비에서 두 날짜를 비교하는 방법을 알아보겠습니다.

## 방법
비교해야 할 두 날짜를 Ruby Date 객체로 만듭니다. 그런 다음 `#<=>` 메소드를 사용하여 이 두 개의 날짜를 비교할 수 있습니다. 이 메소드는 첫 번째 날짜가 두 번째 날짜보다 이전인 경우 -1을, 같은 경우 0을, 이후인 경우 1을 리턴합니다.
```Ruby
require 'date'
date1 = Date.parse("2021-08-25")
date2 = Date.parse("2021-08-30")
puts date1.<=>(date2)
```
위의 코드를 실행하면 콘솔에 `-1`이 출력됩니다. 이는 `date1`이 `date2`보다 이전이라는 것을 의미합니다. 만약 이후인 경우 `date1`과 `date2`를 바꾸면 `1`이 출력됩니다.

비교하는데 있어서 두 번째 날짜를 지정하지 않고 현재 날짜와 비교하려면 `Date.today` 메소드를 사용할 수 있습니다.
```Ruby
require 'date'
date = Date.parse("2021-08-25")
puts date.<=>(Date.today)
```
위의 코드를 실행하면 오늘 날짜와 비교한 결과가 출력됩니다. 만약 오늘 날짜와 같은 경우 `0`이, 이전인 경우엔 `-1`이, 이후인 경우엔 `1`이 출력됩니다.

## 깊게 파고들기
`Date.today` 메소드는 로컬 시간을 기준으로 오늘 날짜를 리턴합니다. 하지만 특정 타임존의 오늘 날짜를 비교하고 싶을 때가 있습니다. 그럴 때는 `Date.today.to_datetime` 메소드를 사용하여 현재 날짜를 DateTime 객체로 만든 다음 `#in_time_zone` 메소드를 사용하여 타임존을 지정할 수 있습니다.
```Ruby
require 'date'
require 'active_support'
current_date = Date.today.to_datetime.in_time_zone("Seoul")
puts current_date
```
위의 코드를 실행하면 한국 표준시 기준으로 현재 날짜를 출력합니다. 이렇게 하면 원하는 타임존의 오늘 날짜를 비교할 수 있습니다.

## 또 다른 정보
- https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-3C-3D-3E
- https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html#method-i-utc
- https://apidock.com/rails/v4.0.2/DateTime/in_time_zone

## 참고
- https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html
- https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html
- https://guides.rubyonrails.org/active_support_core_extensions.html#converting-between-different-natural-date-formats