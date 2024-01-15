---
title:                "두 날짜 비교하기"
html_title:           "Ruby: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 것이 유용한 이유는 많습니다. 예를 들어, 날짜를 비교하여 이전 작업과 비교하거나 휴가 신청일을 확인하는 등 여러 다양한 상황에서 유용하게 사용할 수 있기 때문입니다.

## 어떻게

다음은 Ruby를 사용하여 두 날짜를 비교하는 간단한 예시입니다.

```Ruby
require 'date'
# 날짜를 생성합니다.
date1 = Date.new(2020, 8, 1)
date2 = Date.new(2020, 9, 1)

# == 연산자를 사용하여 두 날짜가 같은지 비교할 수 있습니다.
if date1 == date2
    puts "두 날짜는 같습니다."
else
    puts "두 날짜는 같지 않습니다."
end
```
**출력:**
```
두 날짜는 같지 않습니다.
```

다음은 두 날짜의 차이를 구하는 예시입니다.

```Ruby
require 'date'
# 날짜를 생성합니다.
date1 = Date.new(2020, 8, 1)
date2 = Date.new(2020, 9, 1)

# - 연산자를 사용하여 두 날짜의 차이를 구할 수 있습니다.
diff = date2 - date1
puts "#{diff} 일간의 차이가 있습니다."
```
**출력:**
```
31 일간의 차이가 있습니다.
```

이 외에도 두 날짜를 비교하는 다양한 방법들이 있으며, 자세한 내용은 Ruby 공식 문서를 참고해주세요.

## Deep Dive

Ruby에서는 Date 클래스를 사용하여 날짜를 다룰 수 있습니다. Date 클래스는 날짜, 월, 년을 포함한 다양한 메소드를 제공하며, 이를 활용하여 날짜를 비교하고 관리할 수 있습니다.

Date 클래스의 여러 가지 메소드 중에서도 두 날짜를 비교하는 가장 흔한 방법은 `==` 연산자를 사용하는 것입니다. 이 외에도 `>`, `<`, `<=`, `>=` 연산자를 사용하면 각각 "보다 큰", "보다 작은", "보다 작거나 같은", "보다 크거나 같은" 날짜를 비교할 수 있습니다.

또한, Date 클래스는 오늘 날짜를 구하는 `Date.today` 메소드도 제공합니다. 이를 활용하면 오늘 날짜와 비교하여 일정 기간이 지났는지 등의 여러 가지 작업에 활용할 수 있습니다.

## See Also

- [Ruby 공식 문서](https://ruby-doc.org/core-2.7.2/Date.html)
- [Date 클래스의 사용 예시](https://www.techotopia.com/index.php/Comparing_Date_and_Time_Objects_in_Ruby)