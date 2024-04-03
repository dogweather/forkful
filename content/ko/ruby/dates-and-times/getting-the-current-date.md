---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:47.005461-07:00
description: "\uBC29\uBC95: Ruby\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C\
  \ `Date`\uC640 `Time` \uD074\uB798\uC2A4\uAC00 \uD3EC\uD568\uB418\uC5B4 \uC788\uC2B5\
  \uB2C8\uB2E4. \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uBC29\uBC95\uC740 \uB2E4\
  \uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.011060-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uB294 \uB0A0\
  \uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C `Date`\uC640 `Time`\
  \ \uD074\uB798\uC2A4\uAC00 \uD3EC\uD568\uB418\uC5B4 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
Ruby의 표준 라이브러리에는 날짜와 시간을 다루기 위한 `Date`와 `Time` 클래스가 포함되어 있습니다. 현재 날짜를 얻는 방법은 다음과 같습니다:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

샘플 출력:
```
2023-04-12
```

날짜와 함께 시간을 포함시키고 싶다면, Ruby의 `Time` 클래스가 더 적합합니다:

```ruby
current_time = Time.now
puts current_time
```

샘플 출력:
```
2023-04-12 14:33:07 +0200
```

시간대 관리와 같은 더 많은 기능이 필요한 경우, `ActiveSupport`와 같은 타사 젬을 사용하고 싶을 수 있습니다(`ActiveSupport`는 Rails의 일부이지만 독립적으로 사용할 수 있습니다).

먼저, Gemfile에 `activesupport`를 추가하고 `bundle install`을 실행하세요:

```ruby
gem 'activesupport'
```

그런 다음, 시간대를 처리하기 위해 사용하세요:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # 원하는 시간대 설정
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

샘플 출력:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
