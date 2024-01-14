---
title:    "Rust: 컴퓨터 프로그래밍의 제목 명령 줄 인수 읽기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜

누군가 명령줄 인자를 읽는 것에 참여하려면 *왜* 읽어야 하는지에 대해 1-2 문장으로 설명합니다.

## 어떻게

"```Rust
use std::env; // env 라이브러리를 사용합니다.

fn main() {
  // 커맨드 라인에서 입력된 모든 인자를 벡터로 반환합니다.
  let arguments: Vec<String> = env::args().collect();
  
  // 첫 번째 인자는 항상 실행 파일의 이름이므로 이를 제외하고 인자를 출력합니다.
  for argument in arguments.iter().skip(1) {
    println!("{}", argument);
  }
}
```"

위의 코드는 Rust를 사용하여 커맨드 라인에서 입력된 인자를 읽는 간단한 예제입니다. `env` 라이브러리를 사용하여 `args()` 함수를 호출하면 인자들이 벡터 형태로 반환됩니다. 그리고 `iter()` 함수를 사용하여 벡터의 요소들을 순회하며 출력하면 됩니다.

만약 인자를 숫자로 변환하여 사용하고 싶다면 `parse()` 함수를 사용하면 됩니다. 다음과 같이 `u32`로 변환하는 예제를 보겠습니다.

"```Rust
let num: u32 = argument.parse().unwrap();
```

`unwrap()` 함수는 `parse()` 함수의 결과를 반환합니다. 만약 변환에 실패하면 프로그램이 오류를 발생시키면서 종료합니다.

## 심화 연구

Rust에서 커맨드 라인 인자를 읽는 방법은 여러 가지가 있습니다. `args()` 함수로 전체 인자를 읽는 것 뿐만 아니라 `args_os()` 함수로 인자를 `OsString` 형식으로 읽을 수 있습니다. `args_os()` 함수는 인자를 UTF-8이 아닌 형식으로 읽기 때문에 한국어와 같은 오래된 문자 인코딩을 사용하는 환경에서 유용합니다.

또한 Rust의 `clap` 라이브러리를 사용하면 커맨드 라인 인터페이스를 더욱 유연하게 다룰 수 있습니다. 예를 들어 옵션, 서브 커맨드, 다양한 입력 형식 등을 처리할 수 있습니다.

## 참고 자료

- [Rust 공식 문서의 arg매뉴얼](https://doc.rust-lang.org/std/env/struct.Args.html)
- [범용적인 커맨드 라인 인터페이스 라이브러리인 `clap`](https://clap.rs/)
- [Rust 커맨드 라인 인자를 읽는 다양한 방법들](https://users.rust-lang.org/t/from-str-vs-parsing-when-to-use-which-function-to-convert-from-str-to-other-types/34967)