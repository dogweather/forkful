---
title:                "Ruby: 현재 날짜 가져오기"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍에서 현재 날짜를 가져오는 것은 중요한 작업입니다. 프로그램을 작성하거나 데이터를 분석 할 때 현재 날짜가 필요한 경우가 많아질 수 있습니다. Ruby는 이를 처리하기 위해 내장 라이브러리를 제공하므로 매우 간단하게 작업할 수 있습니다.

## 어떻게?

Ruby에서 현재 날짜와 시간을 가져오는 방법은 매우 간단합니다. 우리는 `Time` 클래스를 사용할 것이며, `now` 메소드를 호출함으로써 현재 시간을 가져올 수 있습니다. 다음은 `Time` 클래스를 사용하여 현재 날짜와 시간을 출력하는 예제 코드입니다.

```ruby
today = Time.now
puts "Today's date is #{today}"
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
Today's date is 2021-06-21 16:46:25 +0900
```
위 코드에서 `#` 기호는 문자열 내에 Ruby 변수 값을 삽입하는 문자열 보간 기법을 사용합니다. 또한 우리는 `Time` 클래스의 `strftime` 메소드를 사용하여 날짜와 시간을 원하는 형식으로 포맷화 할 수 있습니다. 예를 들어, 우리가 오늘의 날짜를 `년-월-일` 형식으로 출력하려면 다음과 같이 코드를 작성할 수 있습니다.

```ruby
today = Time.now
puts "Today's date is #{today.strftime('%Y-%m-%d')}"
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
Today's date is 2021-06-21
```

## 딥 다이브

Ruby의 `Time` 클래스는 우리가 이용할 수 있는 다양한 메소드를 제공합니다. 여기서는 `now` 외에 다른 유용한 메소드들을 살펴보도록 하겠습니다.

### `year`

`year` 메소드는 현재 연도를 정수 형태로 반환합니다. 이를 활용하여 `if` 문과 조합해 특정 연도에 해당하는 작업을 수행할 수 있습니다.

```ruby
today = Time.now
if today.year == 2021
    puts "This is the current year"
end
```

위 코드를 실행하면 `This is the current year`라는 메시지가 출력됩니다. 만약 년도를 비교하는 작업을 자주 해야하는 경우라면 `==` 대신에 `===` 연산자를 사용하는 것이 더 간단한 방법일 수 있습니다.

### `wday`

`wday` 메소드는 주간의 일자를 숫자로 반환합니다. 0은 일요일을 의미하고, 1부터 6까지는 월요일부터 토요일을 의미합니다. 따라서 다음과 같은 코드를 작성하면 현재 요일에 따라 다른 작업을 수행할 수 있습니다.

```ruby
today = Time.now
if today.wday == 0
    puts "Today is Sunday"
elsif today.wday == 6
    puts "Today is Saturday"
else
    puts "Today is a weekday"
end
```

위 코드를 실행하면 `Today is a weekday`라는 메시지가 출력됩니다.

이처럼 `Time` 클래스를 활용하면 현재 날짜와 시간을 다루는 다양한 작업을 할 수 있습니다.

## 관련 링크

- [Ruby `Time` 클래스의 공식 문서](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby에서 현재 날짜와 시간 다루기: `strftime` 예제 코드](https://www.r