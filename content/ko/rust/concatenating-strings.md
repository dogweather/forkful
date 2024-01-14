---
title:    "Rust: 문자열 연결하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것이 중요한 이유는 프로그래밍에서 자주 사용되는 작업이므로, 이 작업을 효율적으로 수행하기 위해 연결 방법을 이해하는 것이 중요합니다.

## 어떻게

Rust에서 문자열을 연결하는 방법은 간단합니다. 기본적으로 `+` 연산자를 사용하면 됩니다.

```Rust
let first_name = "예지";
let last_name = "송";

let full_name = first_name + " " + last_name;
println!("나의 이름은 {}입니다.", full_name);
```

출력:

```
나의 이름은 예지 송입니다.
```

위 예제에서는 `+` 연산자로 두 개의 문자열을 연결했습니다. 하지만 더 많은 문자열을 연결해야 할 수도 있습니다. 이 경우 `format!` 매크로를 사용할 수도 있습니다.

```Rust
let price = 5000;
let currency = "원";
let item = "맛있는 음식";

let message = format!("{} {}에 {}를 살 수 있습니다.", price, currency, item);
println!("{}", message);
```

출력:

```
5000 원에 맛있는 음식를 살 수 있습니다.
```

## 딥 다이브

Rust에서는 문자열을 연결하는 방법으로 세 가지가 있습니다. 첫 번째로, `+` 연산자를 사용하는 방법을 본 이전 예제대로입니다. 두 번째 방법은 `format!` 매크로를 사용하는 것입니다. 마지막으로, `String` 타입의 `push_str` 메소드를 사용하는 방법입니다.

```Rust
let mut hello = String::from("안녕");

hello.push_str("하세요!");
println!("{}", hello);
```

출력:

```
안녕하세요!
```

위 코드에서는 `String` 타입 변수인 `hello`에 `push_str` 메소드를 사용해 문자열을 추가했습니다. 이 방법은 메모리 사용 측면에서 더 효율적입니다.

## 참고

[Rust Book - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)<br>
[Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/primitives/string.html)<br>
[Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)