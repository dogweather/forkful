---
date: 2024-01-26 04:10:15.859766-07:00
description: "\uBC29\uBC95: Rust\uB294 \uC5EC\uB7EC \uB514\uBC84\uAC70\uB97C \uC9C0\
  \uC6D0\uD558\uC9C0\uB9CC, GNU/Linux\uC6A9\uC740 `gdb`, macOS\uC6A9\uC740 `lldb`\uAC00\
  \ \uD754\uD788 \uC0AC\uC6A9\uB429\uB2C8\uB2E4. Rust\uC758 \uAC12\uB4E4\uC744 \uC608\
  \uC058\uAC8C \uCD9C\uB825\uD574 \uC8FC\uB294 \uB798\uD37C\uC778 `rust-gdb`\uB098\
  \ `rust-lldb`\uB97C \uC0AC\uC6A9\uD560 \uC218\uB3C4 \uC788\uC2B5\uB2C8\uB2E4. \uB2E4\
  \uC74C\uC740 \uADF8 \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.921285-06:00'
model: gpt-4-0125-preview
summary: "Rust\uB294 \uC5EC\uB7EC \uB514\uBC84\uAC70\uB97C \uC9C0\uC6D0\uD558\uC9C0\
  \uB9CC, GNU/Linux\uC6A9\uC740 `gdb`, macOS\uC6A9\uC740 `lldb`\uAC00 \uD754\uD788\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

## 방법:
Rust는 여러 디버거를 지원하지만, GNU/Linux용은 `gdb`, macOS용은 `lldb`가 흔히 사용됩니다. Rust의 값들을 예쁘게 출력해 주는 래퍼인 `rust-gdb`나 `rust-lldb`를 사용할 수도 있습니다. 다음은 그 예입니다:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

이를 디버깅하려면 디버그 정보와 함께 컴파일합니다:

```shell
$ rustc -g counter.rs
```

그런 다음 `rust-gdb`에서 실행합니다:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## 심층 탐구
디버깅은 펀치 카드의 *옛 시절*부터 있어왔으며, 그 진화는 신의 한 수였습니다. Rust는 시스템 수준의 특성 때문에 GDB와 LLDB에 대한 통합을 제공하는 자체 도구를 제공합니다.

Rust 코드를 디버깅하는 또 다른 방법으로는 내장 디버거가 있는 통합 개발 환경(IDE)의 사용이 있으며, 일부는 이를 더 직관적으로 찾습니다. 인기 있는 것으로는 Rust 플러그인이 있는 CLion 또는 Rust 확장 프로그램이 있는 Visual Studio Code가 있습니다.

구현에 대해 Rust는 이 디버거들이 이해하는 디버그 심볼을 생성하는데, 이는 코드를 한 단계씩 진행하거나, 중단점을 설정하고, 변수를 검사하는 데 있어서 정신을 잃지 않는 것이 중요합니다.

## 참고
- 디버깅에 관한 러스트 책: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- 에러와 디버깅에 대한 러스트 예제: https://doc.rust-lang.org/rust-by-example/error.html
- VS Code의 Rust 확장을 지원하는 Rust Language Server (RLS): https://github.com/rust-lang/rls
- Visual Studio Code로 Rust 디버깅하기: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
