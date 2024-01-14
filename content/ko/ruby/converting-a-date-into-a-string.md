---
title:                "Ruby: 날짜를 문자열로 변환하기"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜: 날짜를 문자열로 변환하는 방법에 대하여

날짜를 다루는 프로그래밍에서 자주 사용되는 기능 중 하나는 날짜를 문자열로 변환하는 것입니다. 이 기능은 일반적으로 날짜 데이터를 출력하거나 저장할 때 필요하며, Ruby에서도 간단하게 구현할 수 있습니다.

## 어떻게: Ruby에서 날짜를 문자열로 변환하는 방법

아래에 나온 예제들을 참고하여 Ruby에서 날짜를 문자열로 변환하는 방법을 알아보겠습니다.

```Ruby
date = Date.today
puts date.to_s
```
이 코드를 실행하면 오늘의 날짜가 문자열로 출력됩니다. 따로 지정하지 않는 경우에는 기본적으로 "YYYY-MM-DD" 형식으로 출력됩니다.

```Ruby
date = Date.new(2021, 8, 23)
puts date.strftime("%B %d, %Y")
```
이번에는 "strftime" 메소드를 사용하여 원하는 포맷으로 날짜를 출력할 수 있습니다. 위의 예제에서는 월(Month)을 영어로, 일(Day)을 숫자로, 그리고 연도(Year)를 네 자리 숫자로 출력하도록 지정하였습니다.

```Ruby
date = Date.new(2017, 3, 8)
puts date.strftime("%d/%m/%Y")
```
날짜를 출력할 때 "/"를 사용하여 구분할 수도 있습니다. 위의 예제에서는 일과 월의 순서를 바꾸어서 출력하도록 지정하였습니다.

```Ruby
require 'date'

date = "2021-12-25"
puts Date.parse(date).strftime("%A, %B %d")
```
만약 문자열로 된 날짜 데이터를 다루어야 할 경우에는 "parse" 메소드를 사용하여 날짜 객체로 변환한 후, 원하는 포맷으로 출력할 수 있습니다.

## 깊게 들어가보기

Ruby에서는 "strftime" 메소드를 통해 원하는 포맷으로 날짜를 출력할 수 있지만, 날짜를 문자열로 변환하기 전에 날짜 객체를 어떤 형식으로 저장하느냐에 따라 결과가 달라질 수 있습니다. 예를 들어, "Date.today"의 결과를 "2021-8-23"과 같은 형식으로 저장하면 출력할 때 바로 원하는 포맷으로 출력할 수 있지만, "2021-12-25"와 같은 문자열로 직접 지정할 경우에는 날짜 객체로 변환하는 과정이 필요합니다.

그리고 날짜 객체가 아닌 숫자로 된 날짜 데이터를 다룰 경우에도 동일한 방식으로 "parse" 메소드를 사용하여 날짜 객체로 변환한 후, 원하는 포맷으로 출력할 수 있습니다.

## 같이 보기

- [strftime 메소드에 대한 더 자세한 설명](https://ruby-doc.org/core-3.0.2/Time.html#method-i-strftime)
- [날짜와 시간 다루기: Ruby 공식 문서](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [날짜 객체와 문자열 변환에 대한 더 많은 예제들](https://www.rubyguides.com/2015/06/ruby-date-format/)

# 함께 찾아보기

- [날짜 입력 참고: Date 패키지 사용법](https://korea-devlog.tistory.com/7)
- [날짜와 시간 다루기: Ruby 공식 문서 번역](https://rubykr.github.io/classes/Date.html)
- [Ruby에서 날짜와 시간 다