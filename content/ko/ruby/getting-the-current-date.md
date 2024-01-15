---
title:                "현재 날짜 가져오기"
html_title:           "Ruby: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?

현재 날짜를 구하는 것은 프로그래밍에서 매우 일반적입니다. 예를 들어, 회원 가입일, 결제 일자, 또는 파일을 만든 날짜를 표시하는 등의 경우에 많이 사용됩니다.

## 어떻게 하는가?

```Ruby
puts Time.now

# 출력 예시: 2021-03-04 12:30:00 +0900
```

```Ruby
puts Date.today

# 출력 예시:  2021-03-04
```

위의 코드처럼 `Time.now` 또는 `Date.today`를 사용하여 간단하게 현재 날짜와 시간을 출력할 수 있습니다. 이 외에도 `Time`과 `Date` 클래스에는 다양한 메소드가 있으니 필요에 따라 찾아보세요.

## 깊이 파헤치기

`Time.now`와 `Date.today`는 우리가 일상적으로 사용하는 날짜와 시간 형식으로 출력됩니다. 하지만 이를 더욱 세밀하게 다루려면 `strftime` 메소드를 사용할 수 있습니다. 이 메소드는 형식 문자열을 이용하여 날짜와 시간을 원하는대로 출력할 수 있도록 해줍니다.

```Ruby
puts Time.now.strftime("%Y년 %m월 %d일 %H시 %M분 %S초")

# 출력 예시: 2021년 03월 04일 12시 35분 00초
```

또한 `Time`과 `Date` 클래스는 연산이 가능하며, `Time` 클래스는 시간의 차이를 나타내는 `TimeDifference`도 지원합니다. 이를 이용하여 날짜와 시간을 다양하게 조작할 수 있습니다.

## 관련 자료

- [Ruby Time 클래스 문서](https://ruby-doc.org/core-2.7.0/Time.html)
- [Ruby Date 클래스 문서](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [strftime 형식 문자열 참고 문서](https://www.rubyguides.com/2015/05/working-with-dates-in-ruby/)