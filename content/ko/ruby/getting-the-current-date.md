---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:16:13.924673-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

현재 날짜 가져오기는 컴퓨터의 시스템 날짜와 시간을 알아내는 행위입니다. 프로그래머들은 로깅, 사용자 경험 향상, 데이터의 시간적 추적을 위해 이를 사용합니다.

## How to: (방법)

Ruby에서 현재 날짜와 시간을 가져오는 것은 간단합니다. 기본 내장된 `Date`와 `Time` 클래스를 활용해 보세요:

```ruby
require 'date'

# 현재 날짜 출력
puts Date.today
```

```ruby
# 현재 시간을 초 단위로 출력
puts Time.now
```

예상 출력에서 날짜와 시간 형식은 시스템 설정과 로케일에 따라 다를 수 있습니다.

## Deep Dive (심층 분석)

Ruby 언어에는 날짜와 시간을 다루는 몇 가지 방법이 있습니다. `Date` 클래스는 날짜에 초점을 맞추고 `Time` 클래스는 시간도 포함하여 날짜와 시간을 모두 다룹니다.

`DateTime`은 `Time`과 비슷하지만, 고대 날짜와 같은 범위가 넓은 날짜를 다루기 위해 고안되었습니다. 그러나 일상적인 용도로는 `Time`이 더 효율적입니다.

`Time` 객체는 1970년 1월 1일(유닉스 시간의 시작점) 이후의 초를 사용하여 시간을 계산합니다. 이 방식은 프로그래밍에서 표준적으로 사용되며, Ruby에서도 마찬가지입니다.

Ruby의 시간 클래스들은 내부적으로 시스템의 시간 정보를 사용하여 현재 날짜와 시간을 결정합니다. 이는 Ruby 프로그램이 운영되는 시스템의 시간이 정확해야 정확한 값이 보장된다는 것을 의미합니다.

## See Also (참고 자료)

- Ruby 공식 문서: [Time](https://ruby-doc.org/core-3.1.2/Time.html)
- 유닉스 시간에 대한 추가 정보: [Unix Time - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)