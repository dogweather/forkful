---
title:    "Ruby: 현재 날짜 가져오기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

왜 현재 날짜를 얻는 것에 관심을 가져야 할까요? 현재 날짜를 사용하면 프로그래밍을 할 때 많은 기능을 추가할 수 있습니다. 예를 들어, 실제 시간과 날짜를 기반으로 작업을 스케줄링하거나 사용자에게 정확한 시간을 보여주는 기능을 구현할 수 있습니다.

## 방법

1. Ruby의 빌트인 함수를 사용하여 현재 날짜와 시간을 얻을 수 있습니다.

```Ruby
current_date = Time.now
puts current_date
```
출력결과: 2021-09-22 12:00:00

2. 특정 형식으로 날짜를 출력하고 싶다면 `strftime` 메서드를 사용할 수 있습니다.

```Ruby
current_date = Time.now
puts current_date.strftime("%m/%d/%Y")
```
출력결과: 09/22/2021

3. 날짜와 시간을 세분화하여 따로 변수에 저장할 수도 있습니다.

```Ruby
year = current_date.year
month = current_date.month
day = current_date.day
hour = current_date.hour
minute = current_date.minute
second = current_date.second
```

## Deep Dive

날짜와 시간은 컴퓨터에서 숫자로 나타내는 것이 일반적입니다. Ruby에서도 마찬가지로 1970년 1월 1일 자정부터 현재까지 경과한 초를 나타내는 숫자로 날짜와 시간을 나타냅니다. 이를 "Epoch Time"이라고 부릅니다. Ruby에서는 `Time` 클래스의 `now` 메서드를 사용하여 현재 날짜와 시간의 Epoch Time을 가져올 수 있습니다. 이 값을 이용하여 날짜와 시간을 계산하거나 서로 다른 시간대를 변환할 수도 있습니다.

## 더 읽어보기

* [Ruby Time 클래스](https://ruby-doc.org/core-3.0.0/Time.html)
* [Ruby의 strftime 메서드 사용법](https://www.tutorialspoint.com/ruby-time-strftime-method)