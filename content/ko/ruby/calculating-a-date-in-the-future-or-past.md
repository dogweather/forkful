---
title:                "미래나 과거에서의 날짜 계산하기"
html_title:           "Ruby: 미래나 과거에서의 날짜 계산하기"
simple_title:         "미래나 과거에서의 날짜 계산하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 다양합니다. 예를 들어, 이번 달의 마감일을 계산하거나 생일을 알아내는 등 일상 생활에서 유용하게 활용할 수 있습니다.

## 사용 방법

```Ruby
require 'date'

# 오늘 날짜 구하기
today = Date.today

# 미래 날짜 계산하기
future_date = today + 30

# 과거 날짜 계산하기
past_date = today - 7

puts "오늘 날짜는 #{today}, 30일 후는 #{future_date}, 7일 전은 #{past_date}입니다."
```

위의 예제 코드를 실행하면 아래와 같은 결과가 나옵니다.

```
오늘 날짜는 2021-08-21, 30일 후는 2021-09-20, 7일 전은 2021-08-14입니다.
```

위의 코드에서는 Ruby의 `Date` 모듈을 활용하여 오늘 날짜를 구한 뒤, `+`나 `-` 연산자를 이용하여 미래나 과거 날짜를 계산하는 방법을 보여주고 있습니다.

## 딥 다이브

날짜를 계산하는 방법은 Ruby에서 매우 간단합니다. `Date` 모듈을 사용하면 날짜를 다루는 다양한 기능을 쉽게 활용할 수 있습니다. 또한 `DateTime` 모듈을 사용하면 시간까지 함께 계산하는 것도 가능합니다. Ruby는 이와 같은 모듈들을 통해 날짜와 시간을 다루는 기능을 완벽하게 제공하고 있습니다.

## 참고 문서

- [Ruby 공식 문서 - 날짜와 시간 다루기](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/index.html)
- [Ruby Guide - 날짜와 시간 계산하기](https://www.rubyguides.com/2019/02/ruby-date-time-class/)