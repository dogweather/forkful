---
title:    "Ruby: 날짜를 문자열로 변환하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

누군가 날짜를 문자열로 변환하는 데 참여하는 이유는 무엇입니까? 문자열을 다루고 편한 형식으로 날짜를 표현하기 위해서입니다.

## 어떻게

다음은 Ruby를 사용하여 날짜를 문자열로 변환하는 방법을 보여주는 예제입니다. 먼저, `Date` 및 `strftime` 메소드를 사용하여 현재 날짜를 다음 형식으로 문자열로 변환할 수 있습니다.

```Ruby
require 'date'

date = Date.today.strftime("%Y-%m-%d")

puts date
# 결과: 2021-03-05
```

또한 날짜 객체를 직접 문자열로 변환할 수도 있습니다. `to_s` 메소드를 사용하여 다음과 같이 형식을 지정할 수 있습니다.

```Ruby
date = Date.today

puts date.to_s("%B %d, %Y")
# 결과: March 05, 2021
```

위 예제에서 `%B`는 월을 전체 이름으로, `%d`는 날짜를 0으로 패딩한 형식으로, `%Y`는 네 자리 연도로 표시합니다. `%m`은 두 자리로 월을 나타내는 것이므로 이를 사용하면 위에서 보인 예제와 동일한 결과를 얻을 수 있습니다.

## 딥 다이브

Ruby에서는 `strftime` 메소드를 통해 날짜를 다양한 형식으로 변환할 수 있습니다. 다양한 형식 지정자를 사용하여 원하는 형식으로 날짜를 표현할 수 있습니다. 주의해야 할 점은 형식 지정자가 대소문자에 따라 다르게 해석될 수 있다는 것입니다. 예를 들어, `%b`는 약어로 월을 표시하고 `%B`는 전체 이름으로 월을 표시합니다.

날짜를 문자열로 변환할 때 주로 사용하는 형식 지정자 몇 가지를 살펴보겠습니다.

- `%Y`: 네 자리 연도
- `%m`: 두 자리로 월
- `%d`: 두 자리로 날짜
- `%H`: 두 자리로 24시간 표기법으로 시간 (00-23)
- `%M`: 두 자리로 분
- `%S`: 두 자리로 초
- `%b`: 약어로 월
- `%B`: 전체 이름으로 월
- `%a`: 약어로 요일
- `%A`: 전체 이름으로 요일

`strftime` 메소드를 사용하는 것 외에도 `Date` 객체의 `to_s` 메소드를 사용하여 일부 표준 형식으로 날짜를 변환할 수 있습니다. 이는 위에서 언급한대로 `%B %d, %Y`와 같이 직접 형식을 지정하는 대신 표준 형식을 사용하는 것입니다.

더 많은 형식 지정자와 예제는 Ruby 공식 문서에서 확인할 수 있습니다.

## 참고

- [Ruby 공식문서 - Date 클래스](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby 공식문서 - DateTime 클래스](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [strftime 메소드 문서](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [날짜 관련 형식 지정자 목록](https://www.codecademy.com/articles/date-time-format