---
title:                "Ruby: 두 날짜 비교하기"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

두 날짜를 비교하는 것에 대해 생각해보았을 때, 이는 중요한 프로그래밍 작업 중 하나입니다. 예를 들어, 주문을 처리하는 데 필요한 날짜 비교 기능이나 이벤트를 계획하는 데 필요한 날짜 비교 기능과 같은 경우가 있습니다. 그러므로 Ruby를 사용하여 날짜를 비교하는 방법을 배우는 것은 유용할 것입니다.

## 사용 방법

먼저 ```Date``` 클래스의 인스턴스를 만들어야 합니다. 그러기 위해서는 년, 월, 일 수를 매개변수로 전달하여 ```Date.new()```를 호출합니다. 다음은 두 개의 날짜를 비교하는 예제 코드입니다.

```Ruby
date1 = Date.new(2021, 3, 15)
date2 = Date.new(2021, 3, 20)
puts date1 > date2
```
출력:
```
false
```
위의 코드에서는 ```date1```이 ```date2```보다 뒤에 오는 날짜인지를 비교하여 결과로 ```false```가 출력됩니다. 다른 비교연산자인 ```<, <=, >=```을 사용하여 날짜를 비교할 수도 있습니다. 이때 중요한 점은 비교하는 두 날짜 모두 ```Date``` 클래스의 인스턴스여야 한다는 것입니다.

## 깊게 파헤치기

Ruby에서 날짜를 비교하는 데에는 다양한 방법이 있습니다. ```Date``` 클래스의 메소드를 사용하는 것 외에도, 다른 라이브러리나 gem을 사용하여 비교할 수도 있습니다. 예를 들어, ```Chronic```이라는 gem을 사용하면 사람이 읽을 수 있는 형태로 날짜를 비교할 수 있습니다. 또는 ```DateTime``` 클래스를 사용하면 시간까지 고려하여 날짜를 비교할 수 있습니다. 이러한 다양한 방법들을 알아보고 응용할 수 있다면 더 유연하게 날짜를 다룰 수 있을 것입니다.

## 참고 링크

1. [Ruby Date 클래스 문서](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
2. [Ruby 비교 연산자 문서](https://ruby-doc.org/core-2.7.0/Comparable.html)
3. [Ruby DateTime 클래스 문서](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html)
4. [Chronic gem 문서](https://github.com/mojombo/chronic)