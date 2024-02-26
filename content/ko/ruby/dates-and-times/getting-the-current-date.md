---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:47.005461-07:00
description: "\uC5B4\uB5A4 \uD504\uB85C\uADF8\uB798\uBC0D \uC791\uC5C5\uC5D0\uC11C\
  \uB4E0 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294 \uAC83\uC740 \uAC70\
  \uC758 \uD544\uC218\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC5D0\uC11C \uD65C\uB3D9\uC744 \uAE30\uB85D\uD558\uB294 \uAC83\
  \uBD80\uD130 \uB0A0\uC9DC \uC2A4\uD0EC\uD504\uB97C \uD3EC\uD568\uD55C \uBCF4\uACE0\
  \uC11C\uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uAE4C\uC9C0 \uB2E4\uC591\uD569\uB2C8\
  \uB2E4. Ruby\uC5D0\uC11C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uB0A0\uC9DC\uC640 \uAD00\uB828\uB41C \uC791\uC5C5\uC744\
  \ \uC27D\uAC8C \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.990571-07:00'
model: gpt-4-0125-preview
summary: "\uC5B4\uB5A4 \uD504\uB85C\uADF8\uB798\uBC0D \uC791\uC5C5\uC5D0\uC11C\uB4E0\
  \ \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294 \uAC83\uC740 \uAC70\uC758\
  \ \uD544\uC218\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC5D0\uC11C \uD65C\uB3D9\uC744 \uAE30\uB85D\uD558\uB294 \uAC83\uBD80\
  \uD130 \uB0A0\uC9DC \uC2A4\uD0EC\uD504\uB97C \uD3EC\uD568\uD55C \uBCF4\uACE0\uC11C\
  \uB97C \uC0DD\uC131\uD558\uB294 \uAC83\uAE4C\uC9C0 \uB2E4\uC591\uD569\uB2C8\uB2E4\
  . Ruby\uC5D0\uC11C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uB0A0\uC9DC\uC640 \uAD00\uB828\uB41C \uC791\uC5C5\uC744 \uC27D\
  \uAC8C \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
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
