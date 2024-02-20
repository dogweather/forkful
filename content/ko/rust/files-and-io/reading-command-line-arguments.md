---
date: 2024-01-20 17:57:45.139776-07:00
description: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790 \uC77D\uAE30\uB294 \uC0AC\
  \uC6A9\uC790\uAC00 \uCF58\uC194\uC5D0\uC11C \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589\
  \ \uC2DC \uC81C\uACF5\uD558\uB294 \uC635\uC158\uACFC \uD30C\uB77C\uBBF8\uD130\uB97C\
  \ \uC218\uC9D1\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC0AC\uC6A9\uC790\uC758 \uC694\uAD6C\uC0AC\uD56D\uC744 \uBC18\uC601\
  \uD558\uC5EC \uD504\uB85C\uADF8\uB7A8\uC758 \uB3D9\uC791\uC744 \uC870\uC808\uD558\
  \uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.843543
model: gpt-4-1106-preview
summary: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790 \uC77D\uAE30\uB294 \uC0AC\uC6A9\
  \uC790\uAC00 \uCF58\uC194\uC5D0\uC11C \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589 \uC2DC\
  \ \uC81C\uACF5\uD558\uB294 \uC635\uC158\uACFC \uD30C\uB77C\uBBF8\uD130\uB97C \uC218\
  \uC9D1\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC0AC\uC6A9\uC790\uC758 \uC694\uAD6C\uC0AC\uD56D\uC744 \uBC18\uC601\uD558\
  \uC5EC \uD504\uB85C\uADF8\uB7A8\uC758 \uB3D9\uC791\uC744 \uC870\uC808\uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
커맨드 라인 인자 읽기는 사용자가 콘솔에서 프로그램 실행 시 제공하는 옵션과 파라미터를 수집하는 것입니다. 프로그래머들은 사용자의 요구사항을 반영하여 프로그램의 동작을 조절하기 위해 이를 사용합니다.

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
