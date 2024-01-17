---
title:                "미래나 과거에서 날짜 계산하기"
html_title:           "Ruby: 미래나 과거에서 날짜 계산하기"
simple_title:         "미래나 과거에서 날짜 계산하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?: 
날짜를 계산하는 것은 과거나 미래의 특정한 날짜를 계산하는 것을 말합니다. 프로그래머들은 이를 주로 시간에 대한 정보를 처리하기 위해 사용합니다.

## 방법: 
```Ruby
# 현재 날짜를 가져옵니다.
today = Date.today

# 미래의 날짜를 계산합니다.
future_date = today + 7 # 7일 뒤의 날짜를 계산합니다.

# 과거의 날짜를 계산합니다.
past_date = today - 30 # 30일 전의 날짜를 계산합니다.

# 결과를 출력합니다.
puts "오늘: #{today}"
puts "7일 후: #{future_date}"
puts "30일 전: #{past_date}"
```
```
출력 결과:
오늘: 2021-05-20
7일 후: 2021-05-27
30일 전: 2021-04-20
```

## 깊게 들어가기: 
(1) 계산된 날짜는 1583년 이후의 율리우스력 또는 그레고리력을 따르며, 그 이전의 경우 다른 계산 방식이 적용됩니다.
(2) 미래의 날짜를 계산할 때 간편하게 사용할 수 있는 방법으로 가장 먼 과거의 날짜인 4714년 1월 1일부터의 일 수를 기준으로 계산하는 방법도 있습니다.
(3) Date 클래스는 연, 월, 일을 입력받아 날짜 객체를 생성하는데, 이 때 윤년 여부 등 다양한 계산을 실시합니다.

## 관련 정보: 
- [Date 클래스 문서](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [수학적으로 날짜 계산하기](https://medium.com/@omerio/calendrical-calculations-in-ruby-d5991df6f4b2)