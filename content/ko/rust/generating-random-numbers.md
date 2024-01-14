---
title:                "Rust: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤한 수를 생성하는 작업을 하는 이유는 다양합니다. 예를 들어 게임을 만들거나 새로운 데이터를 시뮬레이션하는 등의 프로그래밍 작업을 수행하는 경우, 실제 세계에서 발생하는 랜덤한 요소를 모방하기 위해 랜덤한 수를 사용할 수 있습니다. 또는 암호화나 보안 등의 분야에서도 랜덤한 수는 중요한 요소입니다.

## 어떻게

Rust에서 랜덤한 수를 생성하는 가장 간단한 방법은 `rand` 라이브러리를 사용하는 것입니다. 먼저 `Cargo.toml` 파일에 다음과 같은 의존성을 추가해줍니다.

```
[dependencies]
rand = "0.8.3"
```

그리고 다음과 같은 코드를 사용하여 랜덤한 수를 생성할 수 있습니다.

```Rust
use rand::Rng;

fn main() {
    // 1부터 10 사이의 랜덤한 정수를 생성합니다.
    let random_number = rand::thread_rng().gen_range(1..11);
    println!("랜덤한 수: {}", random_number);
}
```

위 코드를 실행하면, 예를 들어 3이나 8 같은 랜덤한 수가 출력될 것입니다.

## 딥 다이브

`rand` 라이브러리는 여러가지 다양한 메소드를 제공합니다. 예를 들어 `gen_range` 대신 `gen` 메소드를 사용하면, `i32`와 같은 모든 정수 타입이나 `f64`와 같은 모든 부동 소수점 타입을 랜덤하게 생성할 수 있습니다. 또한 `gen_bool` 메소드는 `true` 또는 `false`를 랜덤하게 생성하며, `gen_ascii_chars` 메소드를 사용하면 랜덤한 ASCII 문자열을 생성할 수 있습니다.

이외에도 `rand` 라이브러리는 유용한 기능들을 제공하니, 관련 문서를 참고하여 더 자세한 정보를 얻을 수 있습니다.

## 아래 링크를 확인해보세요

- `rand` 라이브러리 문서: https://docs.rs/rand/0.8.3/rand/
- Rust 공식 웹사이트: https://www.rust-lang.org/
- Rust 커뮤니티 포럼: https://users.rust-lang.org/