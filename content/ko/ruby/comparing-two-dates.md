---
title:    "Ruby: 두 날짜 비교하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것에 대해서 많은 궁금증들이 있을 수 있습니다. 예를 들어, 특정 날짜가 다른 날짜보다 미래인지 과거인지를 알고 싶을 수 있습니다. 이와 같은 궁금한 점들을 해결하기 위해 날짜끼리 비교하는 방법에 대해 배워보도록 하겠습니다.

## 방법

날짜를 비교하기 위해서는 `Date` 클래스에서 제공하는 많은 메소드들을 활용할 수 있습니다. 가장 많이 사용되는 메소드는 `#<=>`입니다. 이 메소드는 두 개의 날짜를 비교하여 왼쪽 날짜가 오른쪽 날짜보다 이전이면 `-1`, 같으면 `0`, 이후이면 `1`의 값을 반환합니다.

```Ruby
irb(main):001:0> require 'date'
=> true

irb(main):002:0> date1 = Date.new(2021, 10, 1)
=> #<Date: 2021-10-01 ((2459475j,0s,0n),+0s,2299161j)>

irb(main):003:0> date2 = Date.new(2021, 9, 1)
=> #<Date: 2021-09-01 ((2459445j,0s,0n),+0s,2299161j)>

irb(main):004:0> date1 <=> date2
=> 1
```

위의 예시에서 `date1`은 `date2`보다 이후인 날짜이기 때문에 `1`의 값을 반환합니다. 이 외에도 `#<` , `#<=` , `#>` , `#>=` 등의 메소드를 사용하여 날짜 비교를 할 수 있습니다.

## 더 들어가보기

실제로 날짜를 비교하다보면 궁금한 상황들이 많이 발생할 수 있습니다. 이를 해결하기 위해 `Date` 클래스에서 제공하는 다른 메소드들도 사용해보면 좋습니다.

예를 들어, 특정 날짜가 어떤 요일인지 알고 싶을 때 `#strftime` 메소드를 사용할 수 있습니다.

```Ruby
irb(main):005:0> date1.strftime("%A")
=> "Friday"
```

위의 예시에서 `%A`는 날짜를 요일로 반환하도록 지시합니다. 이 외에도 다양한 서식 문자를 사용하여 원하는 정보를 출력할 수 있습니다.

## 관련 링크들

- [Ruby 공식 문서 - Date 클래스](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby Monk - Date 클래스](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-more-classes/lessons/118-date)
- [ASAP Utilities - 날짜 비교하기](https://www.asap-utilities.com/rubyscript-manual/how-to-compare-dates.php) 

## 자세히 알아보기

날짜 비교는 프로그램에서 매우 유용하게 쓰이는 기능입니다. 위에서 설명한 것 외에도 다양한 방법으로 날짜를 비교하는 방법이 있으니 관심있는 분은 더 많은 정보를 찾아보시기 바랍니다. 또한 일반적인 날짜 비교뿐만 아니라 시간도 함께 비교하는 것에 대해서도 알아보시면 더욱 다양한 상황에서 활용할 수 있을 것입니다.

## 또 다른 정보

- Ruby의 다른 클래스들에 대해 알고 싶다