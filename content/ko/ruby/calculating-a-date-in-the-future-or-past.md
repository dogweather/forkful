---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Ruby: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜 계산은 미래의 특정 날짜나 과거의 날짜를 찾는 것입니다. 프로그래머들이 날짜 계산을 하는 이유는 오늘로부터 특정 기간이 지난 후의 날짜를 알아내거나, 특정 이벤트가 일어난 지 얼마나 오래되었는지 알아내기 위해서입니다.

## 어떻게 하는가:
```Ruby
require "date"
# 미래의 날짜 계산
future_date = Date.today + 30
puts future_date
# 과거의 날짜 계산
past_date = Date.today - 30
puts past_date
```
이 코드를 실행하면 오늘로부터 30일 후와 30일 전의 날짜를 출력합니다.

## 깊이 있는 분석
1) **역사적 맥락**: 루비는 1995년에 발표된 고급 프로그래밍 언어로, 표준 라이브러리에는 `Date` 클래스를 포함하고 있습니다. 이 클래스는 날짜 계산을 위한 여러 가지 메서드를 제공하고 있습니다.
2) **대안**: 루비의 `Time` 클래스를 사용하여 날짜를 계산할 수도 있습니다. 이 경우, 세부적인 시간 단위까지 계산이 가능합니다.
3) **구현 세부 사항**: `Date.today + 30`은 오늘 날짜에 30일을 더합니다. 이때 더하는 숫자는 하루를 단위로 하는 것입니다. `-`연산은 미래의 날짜에서 현재 날짜를 뺄셈하는 것으로, 과거의 날짜를 계산하게 됩니다.
  
## 참고 자료
다음은 루비의 날짜와 시간에 대해 자세히 설명하는 링크입니다:
1) [루비 공식 문서: Date 클래스](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
2) [루비 공식 문서: Time 클래스](https://ruby-doc.org/core-2.5.1/Time.html)
3) [루비에서 날짜 및 시간 계산하기](https://www.rubyguides.com/ruby-tutorial/ruby-date-time/)