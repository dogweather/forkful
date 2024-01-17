---
title:                "현재 날짜 가져오기"
html_title:           "Rust: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 오늘 날짜는 무엇이고 왜 사용할까요?
먼저, 현재 날짜를 얻는다는 것은 현재 시스템 또는 지역 설정에서 반영된 현재 날짜를 의미합니다. 프로그래머들은 자신의 프로그램에서 날짜와 시간에 대한 정보를 필요로 할 때가 있기 때문에 자주 현재 날짜를 가져오게 됩니다.

## 방법:
```Rust
use std::time::SystemTime;

fn main() {
    // 현재 시간을 얻기 위해 `now()` 함수를 사용합니다.
    let now = SystemTime::now();

    // `now()` 함수의 반환 타입은 `SystemTime` 구조체입니다.
    // 우리는 `ToSystemTime()` 함수를 호출해 `SystemTime` 구조체를 `SystemTime::UNIX_EPOCH`과의 차이로 변환합니다.
    let today = now.duration_since(SystemTime::UNIX_EPOCH).expect("Error in getting today's date.");

    // `today` 변수를 통해 현재 날짜를 출력합니다.
    println!("Today is {} seconds since January 1, 1970.", today.as_secs());
}
```
출력: 
> Today is 1593176424 seconds since January 1, 1970.

## 깊이 들어가보기:
날짜와 시간은 컴퓨터 과학에서 기본적인 요소 중 하나입니다. 오늘날 우리가 사용하는 많은 프로그램들은 다양한 시간대를 지원하고, 여러 다른 날짜 형식을 사용합니다. 이러한 다양한 사용 사례를 지원하기 위해 운영 체제는 일반적인 시간 및 날짜 함수를 제공하고 있습니다.

다양한 언어와 프레임워크에서도 비슷한 기능을 제공합니다. 예를 들어 파이썬에서는 `datetime` 모듈을 통해 날짜와 시간을 다루는 기능을 제공합니다. 하지만 이러한 언어나 프레임워크는 C/C++와 같은 언어보다 훨씬 쉬운 방법으로 날짜와 시간을 다룰 수 있도록 도와줍니다.

현재 날짜를 얻는 방법은 운영 체제의 종류에 따라 다를 수 있습니다. 다만 저희는 Rust 코드를 통해 운영 체제와 무관하게 현재 날짜를 쉽게 얻을 수 있습니다. Rust는 크로스 플랫폼 언어이기 때문입니다.

## 참고:
- [Rust 공식 문서 - std::time 모듈](https://doc.rust-lang.org/std/time/index.html)
- [클레이 존스의 블로그 - 날짜와 시간 다루기](https://craig9.github.io/rust-datetime.html)