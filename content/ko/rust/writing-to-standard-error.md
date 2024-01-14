---
title:    "Rust: 표준 오류에 쓰기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜?

개인 프로젝트를 할 때나 회사에서 개발을 할 때, 우리는 종종 오류 메세지 또는 디버깅 정보를 콘솔에 출력하도록 코드를 작성합니다. 이때 표준 에러 (standard error) 스트림을 사용하여 오류 메세지를 출력할 수 있습니다. 이렇게 함으로써 더 정확한 디버깅이 가능하고, 프로그램의 안정성을 높일 수 있습니다.

## 어떻게?

Rust에서 표준 에러 스트림에 메세지를 출력하기 위해서는 `std::io::stderr()` 함수를 사용해야 합니다. 이 함수는 표준 에러 스트림에 접근할 수 있는 `Stderr` 값으로 결과를 반환합니다. `write!` 매크로를 사용하여 해당 값에 문자열을 씁니다. 마지막으로 `flush()`를 사용하여 버퍼를 비워줍니다.

```Rust
use std::io::{self, Write};

fn main() {
  let mut stderr = io::stderr();  // 스트림에 대한 접근 권한 얻기
  write!(stderr, "오류가 발생했습니다. 관리자에게 문의하세요.").expect("메세지 출력 실패");
  stderr.flush().expect("버퍼 비우기 실패");
}
```

## 깊게 들어가기

표준 에러 스트림은 Rust의 `std::io` 모듈의 일부분입니다. 이 모듈은 입력 및 출력과 관련된 여러가지 기능을 제공합니다. `stdout()` 함수를 사용하면 표준 출력 스트림에 접근할 수 있으며, `stdin()` 함수를 사용하면 표준 입력 스트림에 접근할 수 있습니다. 이와 비슷하게, `stderr()` 함수를 사용하여 표준 에러 스트림에 접근할 수 있습니다.

`Stderr` 값은 `Write` 트레이트를 구현하고 있습니다. 이는 매우 유용한 트레이트로, 쓰기 기능을 제공하는 타입에 구현되며 다양한 데이터 형식을 출력할 수 있게 해줍니다. `write!` 매크로를 사용하여 문자열 뿐만 아니라 다양한 형식의 데이터를 쓸 수 있습니다.

# 참고 자료

- [The Rust Programming Language: Writing to Standard Error](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#writing-error-messages-to-standard-error)
- [The Rust Standard Library Documentation: std::io module](https://doc.rust-lang.org/std/io/index.html)