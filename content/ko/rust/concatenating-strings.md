---
title:                "문자열 연결하기"
date:                  2024-01-20T17:36:11.650731-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
문자열 연결(concatenating strings)이란, 둘 이상의 문자열을 하나로 합치는 것을 뜻해요. 데이터를 하나로 모아서 출력하거나 데이터 처리를 효율적으로 하려고 프로그래머는 문자열을 연결해요.

## How to: (방법)
Rust에서 문자열 연결은 여러 방법으로 할 수 있어요. `+` 연산자를 사용하거나, `format!` 매크로, 그리고 `push`나 `push_str` 메소드 같은 걸 사용할 수 있죠. 아래 예시를 살펴보세요.

```Rust
fn main() {
    let greeting = "안녕".to_string();
    let name = "세계";
    let exclamation = "!";

    // `+` 연산자를 사용
    let hello_world = greeting + " " + name + exclamation;
    println!("{}", hello_world); // 출력: 안녕 세계!

    // `format!` 매크로를 이용
    let hello_world_format = format!("{} {}{}", "안녕", "세계", "!");
    println!("{}", hello_world_format); // 출력: 안녕 세계!

    // `push_str` 메소드 사용
    let mut hello = "안녕".to_string();
    hello.push_str(" 세계");
    hello.push_str(exclamation);
    println!("{}", hello); // 출력: 안녕 세계!
}
```

## Deep Dive (깊이 있는 정보)
Rust에서 문자열 연결은 메모리 관리 측면에서 중요해요. 초기에는 `String` 타입만이 소유권(ownership) 개념에 따라 변경 가능했죠. `+` 연산자나 `format!` 매크로 사용 시 내부적으로 메모리를 재할당하며, 이는 비용이 드는 연산일 수 있어요. 다만, `format!` 매크로는 더 복잡한 형식을 사용할 때 유용해요. `push`와 `push_str`은 mutable `String`에 문자열을 직접 추가하므로, 여러 문자열을 하나씩 추가할 경우엔 더 효율적인 선택일 수 있어요.

## See Also (추가 정보)
- Rust의 공식 문자열 문서: [The Rust Programming Language - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Rust by Example에서 문자열 연결: [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- 메모리 관리에 대한 자세한 설명: [Understanding Ownership in Rust](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)