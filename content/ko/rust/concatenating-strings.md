---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나? 

문자열 연결은 두 개 이상의 문자열을 하나로 합치는 프로세스입니다. 프로그래머들은 데이터를 보다 효과적으로 처리하고, 출력 메시지를 생성하고, 내부 로직에 따라 동적으로 문자열을 조작하기 위해 이를 사용합니다.

## 어떻게 사용하나?

다음은 Rust에서 문자열을 연결하는 방법에 대한 예입니다.

```Rust
let first_string = "안녕하세요, ".to_string();
let second_string = "세상!".to_string();
let greeting = first_string + &second_string;
println!("{}", greeting); // "안녕하세요, 세상!"
```

`format!` 매크로를 사용하여 문자열을 연결할 수도 있습니다.

```Rust
let first_string = "안녕하세요, ".to_string();
let second_string = "세상!".to_string();
let greeting = format!("{}{}", first_string, second_string);
println!("{}", greeting); // "안녕하세요, 세상!"
```

## 깊이 들어가보기

문자열 연결 작업은 프로그래밍 언어가 발전하면서 많은 변화를 겪었습니다. 초기에는 문자열 연결 작업이 비싼 연산으로 간주되어 최적화에 큰 관심이 있었습니다.

Rust에서는 위에서 설명한 방법 이외에도 `push_str()`과 `push()` 메서드를 사용하여 문자열을 추가할 수 있습니다. 이 메서드는 첫 번째 문자열을 변경하므로, 추가적인 메모리 할당 없이 문자열을 연결할 수 있다는 장점이 있습니다.

## 참고 자료

* Rust docs에서 [String](https://doc.rust-lang.org/std/string/struct.String.html) 타입에 대해 자세히 알아보세요.
* [Ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)에 대한 정보, 이것은 Rust에서 문자열 연결을 이해하는 데 중요합니다.
* Rust by Example에서 [Strings](https://doc.rust-lang.org/rust-by-example/std/str.html) 학습.