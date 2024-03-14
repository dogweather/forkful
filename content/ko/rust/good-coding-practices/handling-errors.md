---
date: 2024-01-26 00:57:24.328626-07:00
description: "\uC5D0\uB7EC \uCC98\uB9AC\uB294 \uBB38\uC81C\uAC00 \uBC1C\uC0DD\uD588\
  \uC744 \uB54C \uC774\uB97C \uB2E4\uB8E8\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC608\uC0C1\uCE58 \uBABB\
  \uD55C \uC0C1\uD669\uC744 \uCC98\uB9AC\uD558\uACE0, Rust \uD504\uB85C\uADF8\uB7A8\
  \uC774 \uC791\uC740 \uBB38\uC81C\uC5D0\uB3C4 \uB2F9\uD669\uD558\uC9C0 \uC54A\uACE0\
  \ \uAC15\uB825\uD558\uAC8C \uB3D9\uC791\uD558\uB3C4\uB85D \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.925904-06:00'
model: gpt-4-1106-preview
summary: "\uC5D0\uB7EC \uCC98\uB9AC\uB294 \uBB38\uC81C\uAC00 \uBC1C\uC0DD\uD588\uC744\
  \ \uB54C \uC774\uB97C \uB2E4\uB8E8\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uC608\uC0C1\uCE58 \uBABB\uD55C\
  \ \uC0C1\uD669\uC744 \uCC98\uB9AC\uD558\uACE0, Rust \uD504\uB85C\uADF8\uB7A8\uC774\
  \ \uC791\uC740 \uBB38\uC81C\uC5D0\uB3C4 \uB2F9\uD669\uD558\uC9C0 \uC54A\uACE0 \uAC15\
  \uB825\uD558\uAC8C \uB3D9\uC791\uD558\uB3C4\uB85D \uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

에러 처리는 문제가 발생했을 때 이를 다루는 것입니다. 프로그래머들은 이를 통해 예상치 못한 상황을 처리하고, Rust 프로그램이 작은 문제에도 당황하지 않고 강력하게 동작하도록 합니다.

## 방법:

Rust는 두 가지 주요 방법으로 에러를 처리합니다: 복구 가능한 에러와 복구 불가능한 에러. 두 가지 모두 살펴봅시다.

복구 가능한 에러들은 `Result<T, E>`를 사용합니다:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("파일이 성공적으로 열렸습니다."),
        Err(_e) => println!("파일을 여는 데 실패했습니다."),
    }
}
```

출력 결과는 'hello.txt' 파일에 따라 "파일이 성공적으로 열렸습니다." 또는 "파일을 여는 데 실패했습니다." 중 하나일 것입니다.

복구 불가능한 에러의 경우, 우리는 `panic!`을 사용합니다:

```Rust
fn main() {
    // 이 코드는 파일이 존재하지 않을 가능성이 높기 때문에 프로그램을 패닉상태로 만듭니다.
    let _f = File::open("nowhere.txt").unwrap();
}
```

이 코드를 실행하면 패닉 메시지를 볼 수 있습니다. 프로그램은 바로 멈춰버립니다.

## 심도있게 살펴보기

역사적으로 프로그래밍에서 에러 처리는 혼란스러웠습니다. Rust는 복구 가능한 에러와 복구 불가능한 에러 사이의 명확한 구분을 통해 이를 올바르게 처리합니다.

`Result` 열거형은 복구 가능한 에러를 위한 것입니다. 이는 명시적입니다 - 여러분은 `Ok` 또는 `Err` 변형을 처리합니다. `unwrap()`과 `expect()`와 같은 메서드도 있지만, 이는 `panic!`으로 이어질 수 있는 빠른 해결 방법입니다.

`panic!`은 Rust에서 정말 나쁜 일이 발생했고, 이를 처리할 수 없음을 외치는 방식입니다. 실행을 즉시 멈추는 복구 불가능한 에러와 같습니다. Rust에서의 패닉은 종종 예상하지 못한 버그와 연관이 있는데, 예를 들어 배열의 범위를 벗어나서 인덱싱하는 경우와 같습니다.

에러를 `Result`를 반환하면서 처리하는 것은 에러를 다룰 것으로 예상될 때 선호됩니다. 이는 Rust의 관용적인 방법으로, Rust 개발자들이 합의한 방식입니다. `Option<T>`도 있습니다. 에러가 `Some(T)` 대신 `None`인 경우와 같이, 에러가 단순히 무언가가 없는 경우를 위한 것입니다. 이는 모두 예상치 못한 것을 두려워하지 않고 기대하는 것에 관한 것입니다.

대안이 있나요? 물론, 더 많은 특징이나 더 편리한 사용을 위해 다른 에러 처리 크레이트를 사용할 수 있습니다. `anyhow`는 간단한 에러 처리를 위해, `thiserror`는 라이브러리 코드 내의 에러를 위해 사용됩니다.

## 또한 보기

더 깊게 파고들고 싶으신가요? 여기로 가보세요:

- [Rust Book on Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Rust의 에러 처리 철학을 이해하기 위한 훌륭한 자료입니다.
- [Rust by Example: Error handling](https://doc.rust-lang.org/rust-by-example/error.html) - 직접 손으로 해보며 배울 수 있는 인터렉티브한 예제들이 있습니다.

기억하세요, 좋은 에러 처리는 단지 코딩하는 것이 아니라, 여러분의 코드 사용자를 돌보는 것입니다. 즐거운 코딩 되세요!
