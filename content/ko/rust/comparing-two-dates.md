---
title:    "Rust: 두 날짜 비교하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# 왜

날짜를 비교하는 것에 대해 어떤 이유에서라도 관심이 있을 수 있습니다. 날짜는 우리 일상에서 중요한 역할을 맡고 있으며, 비즈니스 로직이나 알고리즘을 작성할 때 날짜를 비교하는 기능은 필수적입니다. 또한 날짜를 비교하는 것은 프로그래밍 언어를 배우는 과정에서 기본적인 스킬 중 하나입니다. 라스트(Rust)를 배울 때에도 날짜를 비교하는 방법은 중요한 주제 중 하나입니다.

# 방법

```Rust
use chrono::{DateTime, TimeZone, Utc};

fn compare_dates() {
    // 두 날짜 생성
    let first_date = Utc.ymd(2021, 7, 1).and_hms(0, 0, 0);
    let second_date = Utc.ymd(2021, 7, 2).and_hms(0, 0, 0);

    // 두 날짜 비교
    if first_date < second_date {
        println!("첫 번째 날짜가 두 번째 날짜보다 빠릅니다.");
    } else if first_date > second_date {
        println!("첫 번째 날짜가 두 번째 날짜보다 늦습니다.");
    } else {
        println!("두 날짜는 동일합니다.");
    }
}

fn main() {
    compare_dates();
}

// 출력:
// 첫 번째 날짜가 두 번째 날짜보다 빠릅니다.
```

위 코드에서는 라스트의 `chrono` 라이브러리를 사용하여 날짜를 생성하고 비교하는 방법을 보여줍니다. 우선 `DateTime` 구조체를 사용하여 날짜를 생성할 수 있습니다. 그리고 `<`와 `>` 연산자를 사용하여 두 날짜를 비교할 수 있습니다. 예제에서는 첫 번째 날짜가 두 번째 날짜보다 빠르기 때문에 첫 번째 `if` 절이 실행되게 됩니다.

# 딥 다이브

날짜를 비교하는 방법은 여러 가지가 있을 수 있습니다. 위 예제에서는 `DateTime` 구조체를 사용하였지만, `NaiveDateTime` 구조체를 사용하거나 `Duration` 구조체를 사용하는 방법도 있습니다. 또한 날짜 형식에 따라서 비교 방식이 달라질 수도 있습니다. 이러한 다양한 경우에서도 정확히 원하는 결과가 나오도록 날짜를 비교하는 방법을 고민해야 합니다.

# 참고

- [Rust 공식 문서: chrono 라이브러리](https://docs.rs/chrono/)
- [날짜 비교하기 (Rust by Example)](https://doc.rust-lang.org/stable/rust-by-example/std/ops/comparison.html)
- [라스트 튜토리얼: 날짜와 시간 다루기](https://rust-tutorials.github.io/learn-rust-with-examples/8.1-dates-and-times.html)