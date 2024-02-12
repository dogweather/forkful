---
title:                "현재 날짜 가져오기"
aliases:
- /ko/ruby/getting-the-current-date.md
date:                  2024-02-03T19:10:47.005461-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇을, 왜?
어떤 프로그래밍 작업에서든 현재 날짜를 가져오는 것은 거의 필수적인 작업입니다. 애플리케이션에서 활동을 기록하는 것부터 날짜 스탬프를 포함한 보고서를 생성하는 것까지 다양합니다. Ruby에서는 표준 라이브러리를 사용하여 날짜와 관련된 작업을 쉽게 수행할 수 있습니다.

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
