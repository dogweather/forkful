---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:24.575285-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uB0A0\uC9DC\uB97C \uB098\uD0C0\uB0B4\uB294 \uD14D\uC2A4\
  \uD2B8\uB97C Ruby\uAC00 \uC774\uD574\uD560 \uC218 \uC788\uB294 `Date` \uB610\uB294\
  \ `DateTime` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC77C\uC815 \uAD00\uB9AC, \uBD84\uC11D,\
  \ \uB610\uB294 \uB370\uC774\uD130 \uCC98\uB9AC\uC640 \uAC19\uC740 \uC791\uC5C5\uC5D0\
  \uC11C \uB0A0\uC9DC \uBE44\uAD50, \uACC4\uC0B0, \uD615\uC2DD \uC9C0\uC815 \uB4F1\
  \uC758 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574 \uC774\u2026"
lastmod: '2024-03-11T00:14:29.933533-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uB0A0\uC9DC\uB97C \uB098\uD0C0\uB0B4\uB294 \uD14D\uC2A4\uD2B8\
  \uB97C Ruby\uAC00 \uC774\uD574\uD560 \uC218 \uC788\uB294 `Date` \uB610\uB294 `DateTime`\
  \ \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC77C\uC815 \uAD00\uB9AC, \uBD84\uC11D, \uB610\uB294 \uB370\
  \uC774\uD130 \uCC98\uB9AC\uC640 \uAC19\uC740 \uC791\uC5C5\uC5D0\uC11C \uB0A0\uC9DC\
  \ \uBE44\uAD50, \uACC4\uC0B0, \uD615\uC2DD \uC9C0\uC815 \uB4F1\uC758 \uC791\uC5C5\
  \uC744 \uC218\uD589\uD558\uAE30 \uC704\uD574 \uC774\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 날짜를 파싱한다는 것은 날짜를 나타내는 텍스트를 Ruby가 이해할 수 있는 `Date` 또는 `DateTime` 객체로 변환하는 것입니다. 프로그래머는 일정 관리, 분석, 또는 데이터 처리와 같은 작업에서 날짜 비교, 계산, 형식 지정 등의 작업을 수행하기 위해 이 작업을 합니다.

## 방법:
Ruby에서는 표준 라이브러리가 `Date`와 `DateTime` 클래스를 사용하여 문자열에서 날짜를 파싱하는 직접적인 방법을 제공합니다. 다음은 Ruby의 내장 메소드를 사용하는 방법입니다:

```ruby
require 'date'

# 문자열에서 날짜 파싱
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# 좀 더 상세한 시간 표현을 위한 DateTime
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

`parse`가 직접 이해하지 못할 수 있는 형식을 처리하거나 더 많은 제어가 필요한 경우, 형식을 명시적으로 지정하면서 `strptime` (문자열 파싱 시간)을 사용할 수 있습니다:

```ruby
# 커스텀 형식을 위한 strptime 사용
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### 서드 파티 라이브러리 사용:

Ruby의 내장 기능이 강력하지만 때때로 추가 기능이 필요하거나 더 간단한 문법을 선호할 수 있으므로 서드 파티 라이브러리를 선호할 수 있습니다. 자연어 파싱을 위한 인기 있는 선택지는 `Chronic` 젬입니다:

1. 먼저, Chronic을 Gemfile에 추가하고 `bundle install`을 실행합니다:
```ruby
gem 'chronic'
```

2. 그런 다음 다음과 같이 사용합니다:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# 출력은 현재 날짜에 따라 달라질 수 있으며, 2023-04-01에 파싱한다고 가정함
# => 2023-04-04 12:00:00 +0000
```

`Chronic`은 다양한 자연어 날짜 형식을 이해할 수 있어 유연한 날짜 입력이 필요한 애플리케이션에 강력한 도구가 됩니다.
