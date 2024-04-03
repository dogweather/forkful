---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:24.575285-07:00
description: "\uBC29\uBC95: Ruby\uC5D0\uC11C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uAC00 `Date`\uC640 `DateTime` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\
  \uB294 \uC9C1\uC811\uC801\uC778 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . \uB2E4\uC74C\uC740 Ruby\uC758 \uB0B4\uC7A5 \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\
  \uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.009409-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC5D0\uC11C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00\
  \ `Date`\uC640 `DateTime` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\
  \uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294 \uC9C1\uC811\
  \uC801\uC778 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
