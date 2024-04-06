---
date: 2024-01-20 17:57:45.139776-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCEE4\uB9E8\uB4DC\
  \ \uB77C\uC778 \uC778\uC790\uB294 UNIX \uC2DC\uC2A4\uD15C\uC774 \uB4F1\uC7A5\uD558\
  \uBA74\uC11C\uBD80\uD130 \uC0AC\uC6A9\uB418\uC5C8\uC2B5\uB2C8\uB2E4. Rust\uC5D0\uC11C\
  \uB294 `std::env` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC774\uB97C \uCC98\
  \uB9AC\uD569\uB2C8\uB2E4. `env::args()`\uB97C \uD638\uCD9C\uD558\uBA74 \uD504\uB85C\
  \uADF8\uB7A8 \uC774\uB984\uC744 \uD3EC\uD568\uD55C \uBAA8\uB4E0 \uC778\uC790\uB97C\
  \ \uAC00\uC838\uC62C \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uB7EC\uC2A4\uD2B8\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.341912-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uCEE4\uB9E8\uB4DC \uB77C\uC778\
  \ \uC778\uC790\uB294 UNIX \uC2DC\uC2A4\uD15C\uC774 \uB4F1\uC7A5\uD558\uBA74\uC11C\
  \uBD80\uD130 \uC0AC\uC6A9\uB418\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## How to: (어떻게 하나요?)
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        println!("인자 수: {}", args.len() - 1);
        for (index, argument) in args.iter().enumerate().skip(1) {
            println!("인자 {}: {}", index, argument);
        }
    } else {
        println!("인자가 제공되지 않았습니다.");
    }
}
```

실행 결과 예시:
```
$ cargo run arg1 arg2 arg3
인자 수: 3
인자 1: arg1
인자 2: arg2
인자 3: arg3
```

## Deep Dive (심도 있게 파헤치기)
커맨드 라인 인자는 UNIX 시스템이 등장하면서부터 사용되었습니다. Rust에서는 `std::env` 모듈을 사용하여 이를 처리합니다. `env::args()`를 호출하면 프로그램 이름을 포함한 모든 인자를 가져올 수 있습니다. 러스트는 벡터로 인자들을 반환하기 때문에 컬렉션과 반복자에 대한 지식이 필요합니다. 

또한 `clap`, `structopt`, `argh`와 같은 서드파티 크레이트가 있어 더 복잡한 인자 파싱을 위한 강력한 기능을 제공합니다. 이를 사용하면 직관적인 API를 통해 인자 검증, 에러 메시지 관리 등을 수행할 수 있습니다.

## See Also (함께 보기)
- Rust doc for `std::env`: https://doc.rust-lang.org/std/env/
- Clap crate: https://crates.io/crates/clap
- Structopt crate: https://crates.io/crates/structopt
- Argh crate: https://crates.io/crates/argh
