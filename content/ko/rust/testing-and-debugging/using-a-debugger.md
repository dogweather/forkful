---
date: 2024-01-26 04:10:15.859766-07:00
description: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uCF54\
  \uB4DC\uC758 \uC2E4\uD589\uC744 \uB4E4\uC5EC\uB2E4\uBCFC \uC218 \uC788\uB294 \uC5D1\
  \uC2A4\uB808\uC774 \uC2DC\uAC01\uC744 \uC790\uAE30 \uC790\uC2E0\uC5D0\uAC8C \uC81C\
  \uACF5\uD558\uB294 \uAC83\uACFC \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uCC3E\uC544\uB0B4\uACE0, \uD504\uB85C\uADF8\
  \uB7A8\uC758 \uD750\uB984\uC744 \uC774\uD574\uD558\uBA70, \uC790\uC2E0\uC758 \uCF54\
  \uB4DC\uAC00 \uAE54\uB054\uD558\uB2E4\uB294 \uAC83\uC744 \uBCF4\uC7A5\uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uB9C8\uCE58 \uC5B4\uB514\
  \uC11C \uB118\uC5B4\uC84C\uB294\uC9C0 \uC815\uD655\uD788 \uAC00\uB9AC\uCF1C \uC8FC\
  \uB294\u2026"
lastmod: 2024-02-19 22:05:13.826426
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uCF54\uB4DC\
  \uC758 \uC2E4\uD589\uC744 \uB4E4\uC5EC\uB2E4\uBCFC \uC218 \uC788\uB294 \uC5D1\uC2A4\
  \uB808\uC774 \uC2DC\uAC01\uC744 \uC790\uAE30 \uC790\uC2E0\uC5D0\uAC8C \uC81C\uACF5\
  \uD558\uB294 \uAC83\uACFC \uAC19\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBC84\uADF8\uB97C \uCC3E\uC544\uB0B4\uACE0, \uD504\uB85C\uADF8\uB7A8\
  \uC758 \uD750\uB984\uC744 \uC774\uD574\uD558\uBA70, \uC790\uC2E0\uC758 \uCF54\uB4DC\
  \uAC00 \uAE54\uB054\uD558\uB2E4\uB294 \uAC83\uC744 \uBCF4\uC7A5\uD558\uAE30 \uC704\
  \uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uB9C8\uCE58 \uC5B4\uB514\uC11C\
  \ \uB118\uC5B4\uC84C\uB294\uC9C0 \uC815\uD655\uD788 \uAC00\uB9AC\uCF1C \uC8FC\uB294\
  \u2026"
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버거를 사용하는 것은 코드의 실행을 들여다볼 수 있는 엑스레이 시각을 자기 자신에게 제공하는 것과 같습니다. 프로그래머들은 버그를 찾아내고, 프로그램의 흐름을 이해하며, 자신의 코드가 깔끔하다는 것을 보장하기 위해 이를 사용합니다. 마치 어디서 넘어졌는지 정확히 가리켜 주는 친구를 가진 것과 같습니다.

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
