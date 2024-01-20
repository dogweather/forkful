---
title:                "표준 오류에 쓰기"
html_title:           "Rust: 표준 오류에 쓰기"
simple_title:         "표준 오류에 쓰기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 뭐고 왜 하는건데?

표준 에러에 쓰는건 물건 두 가지입니다. 첫째로, 우리는 프로그램에서 발생하는 오류 메시지나 디버그 정보를 사용자에게 표시하기 위해서 사용합니다. 두번째로, 우리는 프로그램에서 예기치 않은 이벤트를 감지하고 조치를 취하기 위해서 사용합니다.

## 어떻게 하나요?

```Rust
use std::io::{self, Write};

fn main() {
    io::stderr().write(b"Hello from standard error!\n").unwrap();
}
```
출력:
```bash
Hello from standard error!
```

## 더 깊이 파고들어보기

표준 에러에 쓰는 것은 매우 오래된 개념입니다. 초기 컴퓨터에서는 디버깅을 위해서 기록장치에 메시지를 출력했습니다. 그 후에는 표준 출력, 표준 에러와 같은 개념을 도입하여 더욱 효율적인 오류 처리를 할 수 있게 되었습니다.
대체로 우리는 표준 에러를 사용하지 않고 대신 로그 파일을 사용할 수도 있지만, 에러가 발생할 때마다 로그 파일을 적절히 갱신하는 것은 비용이 크기 때문에 종종 표준 에러를 사용합니다.
Rust에서는 표준 에러에 쓰는 것을 간단하게 하기 위해 ```io::stderr()``` 함수를 제공합니다.

## 더 알아보기

- [Rust 표준 입력과 출력 라이브러리](https://doc.rust-lang.org/std/io/)