---
title:                "Ruby: 미래 또는 과거 날짜 계산하기"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 시간과 날짜에 관련된 다양한 작업을 수행할 때 매우 유용하기 때문입니다.

## 어떻게

Ruby는 날짜와 시간을 다루기 위한 다양한 메소드들을 제공합니다. 이 중 하나는 `DateTime` 클래스를 이용해 특정 날짜와 시간을 나타낼 수 있다는 것입니다. 또한, Ruby의 `Date` 클래스를 사용하면 날짜를 더 쉽게 다룰 수 있습니다.

```Ruby
# 오늘 날짜 출력
puts Date.today
# 오늘로부터 1년 뒤 날짜 출력
puts Date.today + 1.year
# 오늘부터 3달 전의 날짜 출력
puts Date.today - 3.months
```

위 예시에서 `put` 메소드는 화면에 결과를 출력하는 역할을 합니다. `Date.today`는 현재 날짜를 나타내는 메소드이고, `1.year`는 1년을 의미하며, `3.months`는 3달을 의미합니다. 이렇게 간단하게 날짜를 계산하고 출력할 수 있습니다.

## 딥 다이브

날짜를 미래나 과거로 계산하는 것은 매우 빈번하게 사용되는 기능입니다. Ruby는 이에 편리하게 사용할 수 있도록 다양한 메소드들을 제공합니다. `DateTime`와 `Date` 클래스의 메소드를 자세히 살펴보면 더 많은 기능을 사용할 수 있을 것입니다. 또한, 다른 프레임워크나 라이브러리들도 날짜를 다루기 위한 다양한 옵션을 제공하고 있습니다.

하지만 날짜를 다루는 것 역시 중요한 부분이기 때문에 반드시 정확한 사용법과 문법을 숙지하고 사용해야 합니다. 잘못된 날짜 계산은 예상치 못한 결과를 초래할 수 있기 때문입니다.

## 더 읽어보기

일련의 날짜와 시간을 더 상세하게 다루고 싶다면, 아래의 링크들을 참고해 보세요.

- https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html
- https://ruby-doc.org/stdlib-2.7.2/libdoc/datetime/rdoc/DateTime.html
- https://www.rubyguides.com/2015/06/ruby-date-time-library/
- https://www.rubyguides.com/2017/02/ruby-date/