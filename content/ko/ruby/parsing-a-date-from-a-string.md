---
title:                "문자열에서 날짜 파싱하기"
html_title:           "Ruby: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Ruby: 문자열에서 날짜 파싱하기

## What & Why?
날짜를 문자열에서 추출하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 이 작업은 주어진 문자열에서 날짜를 식별하고 추출하는 과정을 말합니다. 이를 통해 프로그램에서 날짜 데이터를 쉽게 다룰 수 있게 됩니다.

## How to:
Ruby에서 날짜를 문자열에서 추출하는 방법은 간단합니다. 먼저 `Date` 클래스를 불러와야 합니다. 그리고 `parse` 메소드를 사용해서 추출하고자 하는 문자열을 넘겨주면 됩니다.

```ruby
require 'Date'

date_string = "2020-08-26"

date = Date.parse(date_string)
puts date.day #=> 26
puts date.month #=> 8
puts date.year #=> 2020
```

## Deep Dive:
날짜 파싱은 예전부터 프로그래머들에게 익숙한 작업입니다. 예전에는 문자열에서 날짜를 추출하기 위해 정규식을 사용해야 했지만, 이제는 다양한 언어에서 내장된 날짜 파싱 기능을 제공해줍니다.

날짜를 파싱할 때 선택할 수 있는 다른 방법으로는 `DateTime` 클래스를 사용하는 것입니다. 이 클래스는 `DateTime.parse` 메소드를 제공하고 있으며, 시간 정보도 함께 추출할 수 있습니다. 또한 `strptime` 메소드를 사용해서 날짜 포맷을 지정할 수도 있습니다.

## See Also:
- [Ruby `Date` 클래스 문서](https://ruby-doc.org/stdlib-2.6.2/libdoc/date/rdoc/Date.html)
- [Ruby `DateTime` 클래스 문서](https://ruby-doc.org/stdlib-2.6.2/libdoc/date/rdoc/DateTime.html)
- [날짜 파싱 관련 블로그 포스트](https://www.rubyguides.com/2018/12/date-parsing-in-ruby/)