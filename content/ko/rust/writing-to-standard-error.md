---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

category:             "Rust"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
표준 에러(stdout) 쓰기는 프로그램의 오류 메시지를 일반 출력과 분리하는 거예요. 버그 분석이나 로그 기록 때문에 필요해요.

## How to:
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "에러 발생!").unwrap();
}
```

출력:
```
에러 발생!
```

## Deep Dive
표준 에러는 UNIX 시스템에서부터 존재했어요. `println!` 대신 `eprintln!`, `write!` 대신 `ewrite!`를 사용할 수도 있고, `std::io::stderr` 함수로 직접 작성할 수 있어요. 이는 내부적으로 글로벌 'stderr' 핸들을 락하고 씁니다.

## See Also
- [Rust std::io Module](https://doc.rust-lang.org/std/io/)
- [UNIX Stderr Documentation](http://man7.org/linux/man-pages/man3/stderr.3.html)
