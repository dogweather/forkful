---
title:                "Rust: 디버그 출력 출력"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 하는 이유는? 코드를 디버그하는 동안 오류를 찾고 수정하는 것을 도와줍니다.

## 사용 방법

디버그 출력을 하는 방법은 다음과 같습니다:

```Rust
fn main() {
    // 문자열 출력
    println!("안녕하세요!");

    // 변수와 함께 문자열 출력
    let name = "Rust";
    println!("안녕하세요, {}!", name);

    // 숫자 출력
    let num = 42;
    println!("답은 {}입니다.", num);

    // 여러 값 함께 출력
    println!("{} + {} = {}", 2, 3, 2 + 3);
}
```

출력 결과는 다음과 같습니다:

```
안녕하세요!
안녕하세요, Rust!
답은 42입니다.
2 + 3 = 5
```

## 딥 다이브

디버그 출력에 대해 더 깊이 알아보겠습니다. `println!` 매크로는 문자열 이외에도 다양한 타입의 값을 출력할 수 있습니다. 이를 위해 Rust는 `Debug` 트레이트를 제공합니다. `Debug` 트레이트를 구현한 타입은 디버그 출력을 위한 `fmt::Debug` 포맷을 제공합니다.

데이터 구조인 `Person`를 정의하여 `Debug` 트레이트를 구현해보겠습니다:

```Rust
#[derive(Debug)]
struct Person {
    name: String,
    age: i32,
}

fn main() {
    let person = Person{name: String::from("John"), age: 30};
    println!("정보: {:?}", person);
}
```

출력 결과는 다음과 같습니다:

```
정보: Person { name: "John", age: 30 }
```

이렇게 `struct`나 `enum`과 같은 데이터 구조는 `Debug`를 구현하여 `{:?}` 형식으로 출력할 수 있으며, `{:?}` 대신 `{{:#?}}`를 사용하면 보기 좋은 형식으로 출력할 수 있습니다.

## 또 보기

- Rust 표준 라이브러리 문서: https://doc.rust-lang.org/std/
- Rust 프로그래밍 언어: https://www.rust-lang.org/