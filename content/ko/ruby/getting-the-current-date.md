---
title:                "Ruby: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?

현재 날짜를 얻는 것이 왜 유용한지 궁금하신가요? 매일 매일 우리는 일과 업무를 하며 정확한 날짜를 알아야 할 때가 많습니다. 또한 여러분의 프로젝트에서도 날짜와 시간을 다루는 코드가 필요할 수 있습니다.

## 어떻게?

Ruby는 현재 날짜를 얻기 위한 여러 가지 방법을 제공합니다. 그 중 가장 간단하고 일반적으로 사용되는 방법은 `Time.now` 메소드를 사용하는 것입니다.

```Ruby
puts Time.now
```

위의 코드를 실행하면 현재 시간과 날짜가 출력됩니다.

```
2019-09-24 09:00:00 +0900
```

출력된 형식을 변경하고 싶다면 `strftime` 메소드를 사용할 수 있습니다. 이 메소드는 날짜와 시간을 원하는 형식으로 포맷할 수 있도록 해줍니다.

```Ruby
puts Time.now.strftime("%Y년 %m월 %d일")
```

출력 결과:

```
2019년 09월 24일
```

## 깊이 파고들기

우리가 사용하는 컴퓨터 시스템은 현재 시간을 추적하기 위해 "에포크(epoch)"라는 개념을 사용합니다. 에포크는 UTC 시간 기준으로 1970년 1월 1일 자정부터 흐른 시간을 초 단위로 표현하는 것입니다. 이 시간이 0이라는 것은 우리가 어떤 시간도 지정하지 않았다는 것을 의미합니다. 따라서 `Time.now` 메소드를 호출하면 시스템의 에포크 시간을 기준으로 현재 시간이 계산됩니다.

```ruby
puts Time.at(0)
```

출력 결과:

```
1970-01-01 09:00:00 +0900
```

`Time.now` 메소드 대신 `Time.at` 메소드를 사용하면 에포크 시간을 변경하여 다른 날짜와 시간을 출력할 수 있습니다. 또한 `Time.now` 메소드와 마찬가지로 `strftime` 메소드를 사용할 수 있습니다.

## 이외의 참고 링크들

- [Ruby 공식 문서 - Time 클래스](https://ruby-doc.org/core-2.6.3/Time.html)
- [루비를 시작하며 - 시간 다루기](https://www.ruby-lang.org/ko/documentation/quickstart/2/)
- [Ruby 베이직 강좌 - 날짜와 시간 다루기](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)

### 관련 링크

- [Ruby에 날짜/시간 관련 메소드 정리하기](https://github.com/saevonwang/TIL/blob/master/blog-codes/ruby-time-now.rb)
- [더 많은 Ruby 관련 포스팅들](http://rubykorean.blogspot.com/)