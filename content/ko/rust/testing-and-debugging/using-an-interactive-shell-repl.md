---
date: 2024-01-26 04:18:13.057107-07:00
description: "Rust \uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 REPL(Read-Eval-Print\
  \ Loop, \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uB8E8\uD504)\uC740 \uC989\uAC01\uC801\
  \uC778 \uACB0\uACFC\uB97C \uBCF4\uBA74\uC11C Rust \uCF54\uB4DC\uB97C \uC2E4\uD589\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC5B4, \uC2E4\uD5D8\uD558\uAE30\uB098 \uBC30\
  \uC6B0\uAE30\uC5D0 \uC644\uBCBD\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC804\uCCB4 \uD504\uB85C\uC81D\uD2B8\uB97C \uCEF4\uD30C\uC77C\uD558\
  \uB294 \uBD80\uB2F4 \uC5C6\uC774 \uCF54\uB4DC \uC870\uAC01\uC744 \uD14C\uC2A4\uD2B8\
  \uD558\uAC70\uB098\u2026"
lastmod: '2024-03-13T22:44:54.917110-06:00'
model: gpt-4-0125-preview
summary: "Rust \uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 REPL(Read-Eval-Print\
  \ Loop, \uC77D\uAE30-\uD3C9\uAC00-\uCD9C\uB825 \uB8E8\uD504)\uC740 \uC989\uAC01\uC801\
  \uC778 \uACB0\uACFC\uB97C \uBCF4\uBA74\uC11C Rust \uCF54\uB4DC\uB97C \uC2E4\uD589\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC5B4, \uC2E4\uD5D8\uD558\uAE30\uB098 \uBC30\
  \uC6B0\uAE30\uC5D0 \uC644\uBCBD\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC804\uCCB4 \uD504\uB85C\uC81D\uD2B8\uB97C \uCEF4\uD30C\uC77C\uD558\
  \uB294 \uBD80\uB2F4 \uC5C6\uC774 \uCF54\uB4DC \uC870\uAC01\uC744 \uD14C\uC2A4\uD2B8\
  \uD558\uAC70\uB098\u2026"
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 무엇 & 왜?
Rust 인터랙티브 셸, 또는 REPL(Read-Eval-Print Loop, 읽기-평가-출력 루프)은 즉각적인 결과를 보면서 Rust 코드를 실행할 수 있게 해주어, 실험하기나 배우기에 완벽합니다. 프로그래머들은 전체 프로젝트를 컴파일하는 부담 없이 코드 조각을 테스트하거나 디버그하거나 언어 기능을 가지고 놀기 위해 이를 사용합니다.

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
