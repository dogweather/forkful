---
title:                "Ruby: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
날짜를 미래나 과거로 계산하는 것에 참여하려면 왜? 매우 유용하고 간단한 방법입니다. 생일이나 기념일, 예약한 여행 날짜 등을 계산할 수 있습니다.

## 방법
```Ruby
# 미래의 날짜 계산하기
future = Time.now + (60*60*24*7) # 현재 시간보다 일주일 후 날짜 계산
puts future.strftime("%Y년 %m월 %d일") # 연, 월, 일 포맷으로 출력

# 과거의 날짜 계산하기
past = Time.now - (60*60*24*30) # 현재 시간보다 30일 전 날짜 계산
puts past.strftime("%Y년 %m월 %d일") # 연, 월, 일 포맷으로 출력
```

출력:
```
2019년 11월 18일
2019년 10월 20일
```

## 딥 다이브
Ruby의 Time 클래스는 날짜와 시간을 쉽게 계산할 수 있도록 많은 메소드를 제공합니다. 예를 들어, `Time.now`는 현재 날짜와 시간을 보여주고, `strftime("%Y년 %m월 %d일")`는 원하는 포맷으로 날짜를 출력해줍니다. 또한, 덧셈과 뺄셈을 사용하여 미래나 과거 날짜를 계산할 수 있습니다.

## 알아두면 유용한 링크들
[Time 클래스 문서](https://ruby-doc.org/core-2.6.5/Time.html) <br>
[Ruby에서 날짜와 시간 다루기](https://ruby-doc.org/core-2.6.5/DateTime.html) <br>
[광고없이 Ruby 프로그래밍 배우기](https://www.learnruby.online/)