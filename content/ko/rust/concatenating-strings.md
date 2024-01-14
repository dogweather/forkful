---
title:                "Rust: 문자열 연결하기"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열  연결(concatenation) 을 실습하는 것의 이유는 Rust의 효율적이고 강력한 방식을 사용하여 문자열을 다루는 것을 배우기 위함입니다.

## 어떻게

우선, Rust 를 설치하고 실행시키세요. 그리고 다음과 같이 `main.rs` 파일을 만들어서 문자열 연결을 해보세요.

```Rust
fn main() {
  let string1 = "안녕하세요";
  let string2 = "Rust 프로그래밍을 배워봅시다!";
  let result = string1.to_string() + " " + string2;

  println!("{}", result);
}
```

위의 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
안녕하세요 Rust 프로그래밍을 배워봅시다!
```

위의 코드에서 `result` 변수를 보면, 문자열을 연결하기 위해 `+` 연산자를 사용하고 있음을 알 수 있습니다. 이렇게 함으로써 우리는 간단하게 두 개의 문자열을 연결할 수 있습니다.

## 더 깊이 들어가보기

문자열 연결 작업은 Rust에서 여러 가지 방식으로 할 수 있습니다. 위의 예제에서는 `to_string` 함수를 사용해 문자열로 변환한 뒤 `+` 연산자를 사용했습니다. 하지만 이보다 더 효율적이고 강력한 방식으로도 할 수 있습니다.

예를 들어, `String::from` 함수를 사용해서 두 개의 문자열을 연결할 수도 있습니다. 또는 `format!` 매크로를 사용하여 여러 개의 변수를 문자열 내에 삽입하고 연결할 수도 있습니다.

자세한 정보는 [Rust 공식 문서](https://doc.rust-lang.org/std/string/)를 참고하시기 바랍니다.

## 더 알아보기

다른 Rust 기초 개념을 배우고 싶다면 다음의 링크들을 참고해보세요.

- [변수 선언](https://doc.rust-lang.org/stable/book/ch03-01-variables-and-mutability.html)
- [함수](https://doc.rust-lang.org/stable/book/ch03-03-how-functions-work.html)
- [컬렉션](https://doc.rust-lang.org/stable/book/ch08-00-common-collections.html)

## 참고 자료

- [Rust 공식 문서](https://www.rust-lang.org/learn)
- [노마드 코더의 Rust 강좌](https://youtu.be/zF34dRivLOw)
- [Rust 프로그래밍 기초 강의](https://www.youtube.com/watch?v=8kaduiI-JZY)