---
date: 2024-01-20 17:32:15.713091-07:00
description: "How to: Ruby\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uB2E4\uB8E8\uB824\uBA74\
  \ `date` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574\uC57C \uD569\uB2C8\
  \uB2E4. `Date.today`\uB97C \uC774\uC6A9\uD574 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\
  \uACE0, \uC774\uB97C \uAE30\uC900\uC73C\uB85C \uB354\uD558\uAC70\uB098 \uBE7C\uBA74\
  \ \uB429\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.570134-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uB2E4\uB8E8\uB824\uBA74 `date` \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD574\uC57C \uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

## How to:
```Ruby
require 'date'

# 오늘 날짜를 가져옵니다.
today = Date.today
puts "Today is: #{today}"

# 5일 후의 날짜를 계산합니다.
future_date = today + 5
puts "5 days from today will be: #{future_date}"

# 10일 전 날짜를 계산합니다.
past_date = today - 10
puts "10 days ago was: #{past_date}"

# 출력:
# Today is: 2023-03-25
# 5 days from today will be: 2023-03-30
# 10 days ago was: 2023-03-15
```

Ruby에서 날짜를 다루려면 `date` 라이브러리를 사용해야 합니다. `Date.today`를 이용해 현재 날짜를 얻고, 이를 기준으로 더하거나 빼면 됩니다.

## Deep Dive
날짜 계산은 컴퓨팅의 오래된 문제입니다. 초기 컴퓨터 프로그램에서도 날짜는 중요했고, Y2K 버그 같은 문제가 각광을 받기도 했죠. Ruby에서는 `date`와 `time` 라이브러리를 통해 날짜와 시간 계산이 가능합니다. 며칠 더하기나 빼기는 간단하지만, 윤년이나 시간대 변환 같은 복잡한 상황을 처리할 때는 더 신중해야 합니다.

`Date` 클래스 외에 `Time` 클래스나 외부 gem인 `ActiveSupport`(Rails의 일부)도 있습니다. 이들은 각기 다른 기능과 메소드를 제공하기 때문에, 목적에 맞게 선택해서 사용해야 합니다. 예를 들어, 정확한 시간 계산이 필요할 때는 `Time` 클래스가 더 적합할 수 있습니다.

## See Also
- Ruby의 `time` 라이브러리: [Ruby Time Documentation](https://ruby-doc.org/core-2.7.0/Time.html)
- `ActiveSupport`에 관한 정보: [ActiveSupport Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html)

이러한 문서들은 날짜와 시간 계산에 대한 보다 깊은 이해를 위한 좋은 출발점이 될 것입니다.
