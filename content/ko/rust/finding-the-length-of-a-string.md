---
title:    "Rust: 문자열의 길이 찾기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 참여하는 이유는 무엇일까요? 일상에서 우리는 문자열을 다루는 일이 빈번하게 발생하며, 특히 프로그래밍을 할 때에는 문자열의 길이를 알아야 할 때가 많습니다. 따라서 Rust 프로그래밍에서 문자열의 길이를 찾는 방법은 중요한 기술입니다.

## 어떻게

Rust는 기본적으로 UTF-8 인코딩을 사용하기 때문에 문자열의 길이를 단순히 바이트 수로 계산할 수는 없습니다. 따라서 문자열의 길이를 계산하려면 `str` 타입의 `len()` 메서드를 사용해야 합니다. 아래는 문자열의 길이를 구하는 간단한 예제 코드입니다.

```Rust
let my_str = "안녕하세요";
println!("길이: {}", my_str.len());
```

이 코드를 실행하면 `5`라는 결과가 나오게 됩니다. `len()` 메서드는 바이트 단위가 아니라 문자 단위로 문자열의 길이를 계산하기 때문에 올바른 결과를 얻을 수 있습니다.

## 깊숙히 들어가기

`len()` 메서드는 내부적으로 문자열을 반복하여 각 문자의 바이트 수를 계산합니다. 이 때, 문자의 길이는 일반적으로 1바이트가 아니기 때문에 인코딩을 신경써야 합니다. 또한 `len()` 메서드는 `&str` 타입만을 지원하기 때문에 `String` 타입에서는 사용할 수 없습니다.

## 더 많은 정보

문자열의 길이를 얻는 방법은 다양한 Rust 문서와 자료에서 확인할 수 있습니다. 아래 목록에서 참고할 수 있는 링크를 확인할 수 있습니다.

## 더 알아보기

- [Rust 공식 문서: Strings](https://doc.rust-lang.org/std/string/)
- [Rust by Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust Cookbook: Getting String Length](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#getting-string-length)