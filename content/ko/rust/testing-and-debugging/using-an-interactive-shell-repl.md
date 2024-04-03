---
date: 2024-01-26 04:18:13.057107-07:00
description: "\uBC29\uBC95: \uD604\uC7AC\uB85C\uC11C\uB294 Rust\uC5D0 \uACF5\uC2DD\
  \ REPL\uC774 \uD568\uAED8 \uC81C\uACF5\uB418\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. `evcxr_repl`\uACFC\
  \ \uAC19\uC740 \uC81C3\uC790 \uB3C4\uAD6C\uB4E4\uC744 \uC0AC\uC6A9\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4. Cargo\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC124\uCE58\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.917110-06:00'
model: gpt-4-0125-preview
summary: "\uD604\uC7AC\uB85C\uC11C\uB294 Rust\uC5D0 \uACF5\uC2DD REPL\uC774 \uD568\
  \uAED8 \uC81C\uACF5\uB418\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 방법:
현재로서는 Rust에 공식 REPL이 함께 제공되지 않습니다. `evcxr_repl`과 같은 제3자 도구들을 사용할 수 있습니다. Cargo를 사용하여 설치합니다:

```sh
cargo install evcxr_repl
```

그다음, REPL을 실행합니다:

```sh
evcxr
```

내부에서 Rust 코드를 테스트해 보세요:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

출력 결과는 다음과 같아야 합니다:

```
5 + 3 = 8
```

## 심층 분석
Rust의 정신은 안전성과 성능을 중심으로 하며, 이는 일반적으로 컴파일 언어와 관련이 있으며, 해석되는, REPL 친화적인 언어와는 덜 관련이 있습니다. 역사적으로, Python 이나 Ruby 같은 언어들은 즉각적인 피드백을 위한 REPL을 우선시했지만, 시스템 레벨 작업을 염두에 두고 설계된 것은 아니었습니다.

공식 REPL이 Rust에 없음에도 불구하고, `evcxr_repl`과 같은 몇 가지 대안이 등장하였습니다. 이 프로젝트들은 단순히 Rust를 REPL로 해킹하는 것이 아니라, 인터랙티브 세션으로 언어의 컴파일-실행 주기를 똑똑하게 결합하고 있습니다. REPL은 코드를 배후에서 컴파일하고 바이너리를 실행하여 출력을 캡처합니다. 이 방법으로, 인터랙티브한 경험을 제공하면서도 Rust의 성능 이점을 유지합니다.

Rust 커뮤니티 내에서 공식 REPL 지원에 대한 지속적인 토론이 있으며, 언어의 각 반복마다 우리는 결국 네이티브 솔루션으로 이어질 수 있는 더 많은 도구의 정교함을 볼 수 있습니다.

## 참고
더 많은 정보 및 다른 도구들을 위해서:
- Evcxr REPL GitHub 레포: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, Rust 코드를 테스트할 수 있는 온라인 방법: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- REPL 기능에 대한 Rust 언어 논의: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
