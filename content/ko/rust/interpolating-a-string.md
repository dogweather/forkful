---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간은 변수, 상수, 식 등을 문자열 안에 기재하는 것을 의미하며 프로그래머들은 이를 토대로 보다 손쉽게 변수의 값 나타내고, 코드의 가독성을 높이기 위해 사용합니다.

## 방법:

다음은 Rust 언어에서 문자열 보간을 진행하는 방법을 안내하는 코드 예제입니다.

```Rust
fn main() {
    let name = "Peter";
    let age = 23;
    println!("Hello, my name is {} and I am {} years old.", name, age);
}
```

이 코드를 실행하면 결과는 다음과 같이 출력됩니다.

```Rust
Hello, my name is Peter and I am 23 years old.
```

## 깊게 알아보기: 

1. 연혁: 문자열 보간 기법은 오래 전부터 다양한 프로그래밍 언어에서 사용되어 왔습니다. Rust와 같은 현대적인 언어에서는 이를 보다 간소화하고, 사용자 친화적으로 구현하였습니다.
2. 다른 방법: 문자열과 변수를 더하거나 결합하는 방식으로도 간단한 문자열 보간을 할 수 있지만, Rust에서는 `format!` 매크로를 활용하여 간결하게 표현할 수 있습니다.
3. 구현 세부정보 :  Rust에서 문자열 보간은 `println!` or `format!` 매크로를 통해 처리됩니다. 이들은 변수나 표현식을 문자열로 만들어 문자열에 직접 삽입할 수 있도록 합니다.

## 참고 자료:

- [Rust 프로그래밍 공식 가이드](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/)
- [Rust에 대한 Stackoverflow 토론](https://stackoverflow.com/questions/tagged/rust)