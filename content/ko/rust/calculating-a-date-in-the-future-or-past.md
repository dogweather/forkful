---
title:                "미래나 과거에서 날짜 계산하기"
html_title:           "Rust: 미래나 과거에서 날짜 계산하기"
simple_title:         "미래나 과거에서 날짜 계산하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#오늘 무슨 날짜야? 날짜 계산을 배우자!

## 무엇인가요?
우선, 날짜 계산이 무엇인지 알아보겠습니다. 여러분들이 어떤 날짜를 입력하면, 그 날짜의 미래 또는 과거의 날짜를 계산해주는 것을 말해요. 이 작업을 프로그래머들은 자주 사용하는데, 예를 들어서 어떤 이벤트를 몇 일 후에 발생시키고 싶을 때나, 어떤 데이터를 정해진 날짜에 만료되게 하기 위해서 등등 많은 경우에 사용하죠. 고맙게도 러스트(Rust)에서는 이 작업을 쉽게 할 수 있어요!

## 어떻게 하나요?
자, 이제 직접 코드를 작성해보겠습니다! 이번에는 미래의 날짜를 계산하는 예제를 살펴볼 거예요. 아래에 보이는 코드를 그대로 따라하며 실행해보세요.

```
Rust fn main() {
    use chrono::{Duration, Local, DateTime};
    let now = Local::now();
    let future_date = now + Duration::days(7);
    println!("In one week, it will be {}", future_date.format("%B %d, %Y"));
}
```

위 코드를 실행하면 "In one week, it will be [날짜]" 라는 메시지가 출력될 거예요. 여기서 [날짜] 자리에는 현재 날짜로부터 일주일 뒤의 날짜가 출력될 거예요. 이처럼 러스트에서는 날짜 계산을 위해 크로노 (chrono)라는 라이브러리를 사용합니다.

## 깊이 파고들어보기
날짜 계산은 오래 전부터 컴퓨터 프로그래밍에서 매우 중요한 역할을 하고 있었어요. 현재는 우리가 흔히 사용하는 그레고리력(calendar)을 기반으로 날짜를 계산하기 때문에, 이 또한 매우 오래된 역사를 가지고 있어요. 러스트 이외에도 파이썬(Python)이나 자바(Java) 등 다른 프로그래밍 언어에서도 날짜 계산을 위한 다양한 라이브러리가 있습니다. 그리고 날짜 계산을 위해 다양한 알고리즘과 방법들이 개발되었는데, 이 역시 프로그래밍을 배울 때 중요한 부분 중 하나입니다.

## 더 알아보기
날짜 계산에 대해 더 알고 싶다면, 아래의 링크들을 참고해보세요!

- [Chrono documentation](https://docs.rs/chrono/latest/chrono/)
- [History of calendars](https://www.timeanddate.com/calendar/info.html)
- [Python datetime library](https://docs.python.org/3/library/datetime.html)
- [Java Calendar class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)