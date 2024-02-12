---
title:                "로깅"
aliases:
- ko/rust/logging.md
date:                  2024-01-26T01:09:15.961601-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/logging.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

로깅은 애플리케이션을 위한 일기장과 같습니다. 로깅은 런타임 동안 이벤트, 오류, 그리고 다른 관련 데이터를 기록하는 행위입니다. 개발자들은 문제를 진단하고, 시스템 행위를 모니터링하며, 개선을 이끌어내는 통찰력을 얻기 위해 로그를 사용합니다. 이것은 운영 지능의 기본 중의 기본입니다.

## 방법:

`log` 크레이트를 사용하여 Rust에서 기본 로깅 시나리오를 설정해 보겠습니다. `log` 크레이트를 위한 로깅 파사드를 제공하며, `env_logger`는 `log` 크레이트를 위한 로깅 구현체입니다. 먼저, 이들을 Cargo.toml에 추가하세요:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

이제, `main.rs`에서 로거를 설정하고 초기화하세요:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("이것은 정보 메시지입니다.");
    warn!("이것은 경고 메시지입니다.");
}
```

앱을 `RUST_LOG=info cargo run`으로 실행하면, 출력을 볼 수 있습니다:

```
INFO: 이것은 정보 메시지입니다.
WARN: 이것은 경고 메시지입니다.
```

`RUST_LOG` 환경 변수를 'error', 'warn', 'info', 'debug', 'trace'로 설정하여 로그의 상세 수준을 조절해 봅니다.

## 깊이 있게 살펴보기

로깅이라는 개념은 새로운 것이 아니며, 컴퓨팅의 초기 시절부터 존재해 왔습니다. 소프트웨어에서 로깅이 보편화되기 전에 개발자들은 프로그램 실행을 추적하기 위해 print 문이나 디버거 도구와 같은 원시적인 방법에 의존했습니다. 프로그램이 복잡해지면서 로깅에 대한 체계적인 접근 방식이 필요해졌습니다.

Rust에서, `log` 크레이트는 로깅 구현 세부 사항을 추상화하여 개발자가 다양한 로깅 백엔드를 플러그인할 수 있게 해줍니다. `env_logger`는 흔히 사용되는 선택지이지만, `fern`, `slog`, 또는 `tracing`과 같은 다른 선택지들도 있으며 각각 고유한 기능과 구성 옵션을 가지고 있습니다.

로깅을 구현할 때 고려해야 할 몇 가지 사항은 다음과 같습니다:

1. **로그 레벨**: 상세 수준을 조절하는 것이 중요합니다. Rust의 `log` 크레이트는 여러 로그 레벨을 정의합니다: error, warn, info, debug, trace가 있으며 심각도가 줄어드는 순서입니다.

2. **성능**: 로깅은 성능에 영향을 줄 수 있습니다. 성능에 중요한 경로에서 로깅을 피하고 생산 환경에서 지나치게 상세한 로그를 피하는 등 신중하게 사용하는 것이 중요합니다.

3. **구조화된 로깅**: 현대의 모범 사례에는 JSON 같은 기계가 읽을 수 있는 형식으로 작성되는 구조화된 로깅이 포함됩니다. `slog`와 같은 라이브러리는 Rust에서 구조화된 로깅을 가능하게 하며, ELK Stack이나 Splunk와 같은 로그 관리 시스템을 사용해 인덱싱하고 쿼리할 수 있습니다.

4. **비동기 로깅**: 메인 애플리케이션에 영향을 최소화하기 위해서 로깅은 비동기적으로 수행될 수 있습니다. 이는 종종 로깅 라이브러리가 인메모리 큐에 기록하고 별도의 쓰레드가 큐를 처리하고 로그를 목적지에 기록하는 방식으로 달성됩니다.

5. **구성**: 많은 로깅 프레임워크는 환경 변수, 구성 파일, 그리고/또는 코드를 통한 구성을 지원합니다. 이러한 유연성은 다양한 환경(개발, 스테이징, 생산)에서 출력을 세밀하게 조정하는 데 핵심적입니다.

## 또한 보기

- `log` 크레이트 문서: https://docs.rs/log/
- `env_logger` 크레이트 문서: https://docs.rs/env_logger/
- Rust by Example 로깅 페이지: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- `slog` 크레이트, 다른 로깅 프레임워크: https://github.com/slog-rs/slog
- Tracing, Rust 프로그램을 계측하기 위한 프레임워크: https://crates.io/crates/tracing
