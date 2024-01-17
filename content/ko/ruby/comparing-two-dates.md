---
title:                "두 날짜 비교하기"
html_title:           "Ruby: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 무슨 것이며 왜?
두 날짜를 비교하는 것은 간단한 작업처럼 보일 수 있지만, 프로그래머들에게 매우 중요한 일 입니다. 두 날짜를 비교하여 언제 이전 날짜가 더 빠른지 혹은 같은지, 그리고 얼마나 차이가 있는지를 알 수 있습니다. 이는 날짜와 시간을 다루는 소프트웨어에서 필수적인 기능입니다.

## 하는 방법:
Ruby에서 두 날짜를 비교하는 방법은 매우 간단합니다. ``` Date#<=> ``` 메서드를 사용하여 두 날짜를 비교할 수 있고, ```DateTime#-``` 메서드를 사용하여 두 날짜의 차이를 구할 수 있습니다. 아래는 간단한 예시 코드와 그 결과입니다.

```Ruby
require 'date'
date1 = Date.new(2021, 6, 1)
date2 = Date.new(2021, 6, 5)

# 두 날짜를 비교하기
puts date1 <=> date2 # 출력: -1 (date1이 date2보다 이전 날짜)

# 두 날짜의 차이 구하기
puts date1 - date2 # 출력: -4 (4일 차이)
```

## 깊게 들어가기:
두 날짜를 비교하는 방법은 Ruby가 아닌 다른 언어에서도 비슷합니다. 그러나 Ruby에서는 두 날짜를 비교할 때 기본적으로 Julian calendar를 이용합니다. 그리고 ```Date#class``` 메서드를 사용하여 주어진 날짜가 윤년인지 아닌지, 즉 윤년을 따로 처리할 필요가 없습니다. 또한 ```Date#<=>``` 메서드는 ```Date#==``` 메서드와 관련이 있는데, 이것은 날짜만 비교하는 것이 아니라 시간까지 모두 고려하여 비교합니다.

## 관련 정보:
- [Ruby Date Class](https://ruby-doc.org/stdlib-2.6.6/libdoc/date/rdoc/Date.html)
- [Ruby DateTime Class](https://ruby-doc.org/stdlib-2.6.6/libdoc/date/rdoc/DateTime.html)
- [Date and Time Comparisons in Ruby](https://blog.makandra.com/2009/12/date-and-time-comparisons-in-ruby/)