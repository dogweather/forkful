---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:37:43.357823-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환하는 것은, 단순히 날짜 객체를 텍스트 형태로 표현하는 과정입니다. 프로그래머들은 데이터 포맷을 통일하거나 날짜를 사용자 친화적으로 전시하기 위해 이 변환을 사용합니다.

## How to: (어떻게 하나요?)
Ruby에서 날짜를 문자열로 변환하려면 `to_s` 또는 `strftime` 메소드를 사용합니다. 간단한 예제를 보여드리죠:

```Ruby
require 'date'

# 현재 날짜 가져오기
current_date = Date.today

# 기본 변환
date_string = current_date.to_s
puts date_string  # => "2023-04-10"

# 사용자 정의 형식으로 변환
formatted_date_string = current_date.strftime("%Y년 %m월 %d일")
puts formatted_date_string  # => "2023년 04월 10일"
```

`strftime` 메소드에서 `%Y`, `%m`, `%d`는 각각 '년', '월', '일'을 나타냅니다.

## Deep Dive (깊이 파고들기)
날짜를 문자열로 변환하는 기능은 많은 프로그래밍 언어에서 오랜 역사를 가지고 있습니다. Ruby에서 `strftime` 메소드는 퍼포먼스와 유연성 모두를 제공합니다. 이 메소드는 시간 데이터를 다양한 형태로 포맷할 수 있게 해 주며, 로케일별 날짜 표기법을 지원하기 위한 많은 옵션을 포함하고 있습니다.

`to_s` 메소드는 표준 ISO 8601 형식 (yyyy-mm-dd) 으로 날짜를 변환하며, 간단히 사용할 때 적합합니다. 하지만, 더 상세한 제어가 필요할 경우 `strftime` 메소드가 훨씬 유용합니다.

대안으로, 일부 Ruby 라이브러리는 날짜를 문자열로 변환하는 추가 기능을 제공합니다. 예를 들어, `active_support/core_ext`에서 확장된 `to_s` 변형을 사용할 수 있으며, 이는 Rails 프레임워크에서도 사용됩니다.

## See Also (관련 자료)
- `strftime` 메소드에 대한 상세 문서: [https://ruby-doc.org/core/Time.html#method-i-strftime](https://ruby-doc.org/core/Time.html#method-i-strftime)
- Active Support의 시간 확장: [https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)