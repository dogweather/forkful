---
title:                "정규 표현식 사용하기"
aliases:
- /ko/rust/using-regular-expressions/
date:                  2024-02-03T19:18:33.397733-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식, 혹은 regex는 개발자가 고급 패턴 매칭 기술로 문자열을 검색, 매칭, 조작할 수 있게 해줍니다. Rust에서 regex를 활용하면 텍스트 데이터의 파싱과 처리가 효율적으로 이루어져 데이터 검증, 검색, 텍스트 변환과 같은 작업을 더 간결하고 유지보수가 용이하게 만들어줍니다.

## 사용 방법:

Rust의 `regex` 라이브러리는 정규 표현식을 다루기 위한 필수 요소입니다. 이를 사용하기 위해 먼저 `Cargo.toml`에 추가해야 합니다:

```toml
[dependencies]
regex = "1"
```

그 후, Rust 코드에서 regex 기능을 구현하기 시작할 수 있습니다. 여기 몇 가지 일반적인 작업을 수행하는 방법을 소개합니다:

### 문자열에서 패턴 매칭

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("해당 텍스트가 날짜 패턴과 일치합니까? {}", re.is_match(date));
    // 출력: 해당 텍스트가 날짜 패턴과 일치합니까? true
}
```

### 매치 찾기 및 접근하기

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("언어: {}, 연도: {}", &cap[1], &cap[2]);
    }
    // 출력:
    // 언어: Rust, 연도: 2023
    // 언어: C++, 연도: 2022
    // 언어: Python, 연도: 2021
}
```

### 텍스트 교체하기

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1가 $2년에 업데이트되었습니다");

    println!("업데이트된 텍스트: {}", replaced);
    // 출력: 업데이트된 텍스트: Rust가 2023년에 업데이트되었습니다, C++가 2022년에 업데이트되었습니다, Python이 2021년에 업데이트되었습니다
}
```

### 정규 표현식을 사용한 텍스트 분할

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // 모든 비단어 문자에서 분할
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("언어: {}", field);
    }
    // 출력:
    // 언어: Rust
    // 언어: C++
    // 언어: Python
    // 언어: Go
}
```

이 예제들은 Rust에서 정규 표현식을 시작하는 기본 가이드를 제공합니다. 당신의 요구가 더 복잡해짐에 따라, `regex` 크레이트는 복잡한 패턴 매칭과 텍스트 조작 작업을 위한 풍부한 기능을 제공합니다.
