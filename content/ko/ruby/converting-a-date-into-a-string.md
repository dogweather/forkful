---
title:                "날짜를 문자열로 변환하기"
html_title:           "Ruby: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것의 중요성은 여러 가지 이유가 있습니다. 가장 일반적인 이유는 날짜를 특정 형식에 맞추기 위해서입니다. 또한, 문자열로 된 날짜를 다른 언어나 플랫폼에서도 인식할 수 있습니다.

## 하는 법

Ruby에서 날짜를 문자열로 변환하는 방법은 간단합니다. `strftime()` 메소드를 사용하면 됩니다. 이 메소드는 날짜 객체와 형식 지정자를 입력으로 받아서 문자열로 변환합니다. 예시를 살펴보겠습니다.

```Ruby
require 'date'

# 오늘 날짜를 가져옵니다.
today = Date.today

# 연월일 형식으로 출력합니다.
puts today.strftime("%Y-%m-%d")
# 결과: 2021-01-01

# 월/일/년도 형식으로 출력합니다.
puts today.strftime("%m/%d/%Y")
# 결과: 01/01/2021
```
이처럼 `strftime()` 메소드를 사용하면 원하는 형식으로 날짜를 문자열로 변환할 수 있습니다. 그리고 바로 변환된 문자열을 출력할 수 있습니다.

## 깊이 들어가기

`strftime()` 메소드는 C언어의 `strftime()` 함수에서 따온 것입니다. 그래서 Ruby의 형식 지정자들도 C언어의 것들과 유사합니다. 다음은 주로 사용되는 형식 지정자들과 그 의미입니다.

- `%Y`: 4자리 연도
- `%m`: 2자리 월 (01-12)
- `%d`: 2자리 일 (01-31)
- `%H`: 24시간 형식의 시 (00-23)
- `%M`: 분 (00-59)
- `%S`: 초 (00-59)
- `%a`: 요일의 약어 (Sun-Sat)
- `%A`: 요일의 전체 이름 (Sunday-Saturday)
- `%b`: 월의 약어 (Jan-Dec)
- `%B`: 월의 전체 이름 (January-December)
- `%p`: AM/PM (AM/PM)
- `%Z`: 타임존 (e.g. UTC, EST, KST)

더 많은 형식 지정자들을 확인하고 싶다면 [Ruby 공식 문서](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)를 참고해주세요.

## See Also

- [Ruby DateTime Formatting Guide](https://www.rubyguides.com/2015/01/ruby-datetime/)
- [Ruby Date and Time Class](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html#class-Date-label-Conversion+to+other+date+forms)