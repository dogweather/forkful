---
title:    "Ruby: 두 날짜 비교하기"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# 왜
두 개의 날짜를 비교하는 것이 왜 중요한지에 대한 설명은 다양합니다. 유용한 예시로는 프로젝트에서 날짜별로 정렬된 이메일을 필터링하거나 블로그에서 최신 게시물을 표시하는 등 여러 분야에서 사용될 수 있습니다.

이러한 작업을 수행하는 데에는 다양한 방법이 있지만, 루비를 사용하여 날짜를 비교하는 것은 간단하고 효율적입니다. 이제 루비를 사용하여 두 개의 날짜를 비교하는 방법을 알아보겠습니다.

## 방법
날짜를 비교하는 가장 간단한 방법은 `Date#<=>` 메서드를 사용하는 것입니다. 이 메서드는 두 개의 날짜를 비교하여 비교 결과를 양수, 음수 또는 0의 값으로 반환합니다. 예를 들어, 두 개의 날짜를 변수에 할당한 다음 `<=>` 메서드를 사용하여 비교하면 다음과 같은 결과를 얻을 수 있습니다.

```Ruby
date1 = Date.new(2021, 4, 1)
date2 = Date.new(2021, 3, 1)

puts date1 <=> date2
```

이 코드를 실행하면 `-1`이라는 결과 값이 나오는 것을 볼 수 있습니다. 이는 `date1`이 `date2`보다 이전 날짜임을 의미합니다. 이와 같이 `<=>` 메서드를 사용하여 비교하면 두 날짜를 쉽게 비교할 수 있습니다.

다른 방법으로는 `Comparable` 모듈을 사용하는 것입니다. 이 모듈을 include한 클래스에는 `<=>` 메서드가 자동으로 정의되어 있기 때문에, 다음과 같이 코드를 작성하여 비교를 할 수 있습니다.

```Ruby
class Event
  include Comparable

  attr_reader :title, :date

  def initialize(title, date)
    @title = title
    @date = date
  end

  def <=>(other)
    @date <=> other.date
  end
end

event1 = Event.new("RubyConf", Date.new(2021, 8, 28))
event2 = Event.new("JavaScript Conference", Date.new(2021, 9, 25))

puts event1 > event2
```

이 코드를 실행하면 `false`라는 결과 값이 나오는 것을 볼 수 있습니다. 이는 `event1`이 `event2`보다 이전 날짜임을 의미합니다. 이와 같이 객체 내에서 날짜를 비교하는 방법은 다양하지만, 이 두 가지를 사용하면 쉽게 날짜를 비교할 수 있습니다.

## 깊게 파고들기
더 많은 정보를 원하는 경우, `Date` 클래스와 `Time` 클래스에는 다양한 메서드가 있으므로 공식 문서를 참조하는 것이 좋습니다. `Date` 클래스에는 `Date#tomorrow`, `Date#yesterday`, `Date#next_month`, `Date#next_year` 등 다양한 메서드가 있습니다.

또한 `Date`와 `Time` 클래스는 `strftime` 메서드를 사용하여 날짜 포맷을 지정할 수 있습니다. 이를 사용하면 날짜를 원하는 형식으로 표시할 수 있습니다. 예를 들어, `"%Y-%m-%d"`와 같은 형식을 사용하면 `YYYY-MM-DD`와 같은 날짜 형식으로 표시됩니다.

## 더 알아보기
- [Ruby 공식 문서 - Date 클래스](https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html)
- [Ruby