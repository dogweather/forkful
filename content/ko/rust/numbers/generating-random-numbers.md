---
date: 2024-01-27 20:35:28.045155-07:00
description: "Rust\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740 \uC554\uD638\uD559\
  , \uC2DC\uBBAC\uB808\uC774\uC158, \uAC8C\uC784 \uBC0F \uBB34\uC791\uC704 \uC54C\uACE0\
  \uB9AC\uC998\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC791\uC5C5\
  \uC5D0 \uD544\uC218\uC801\uC778 \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC22B\uC790\
  \ \uAC12\uC744 \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uD65C\uC6A9\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.909215-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C \uB09C\uC218 \uC0DD\uC131\uC740 \uC554\uD638\uD559, \uC2DC\
  \uBBAC\uB808\uC774\uC158, \uAC8C\uC784 \uBC0F \uBB34\uC791\uC704 \uC54C\uACE0\uB9AC\
  \uC998\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC791\uC5C5\uC5D0\
  \ \uD544\uC218\uC801\uC778 \uC608\uCE21\uD560 \uC218 \uC5C6\uB294 \uC22B\uC790 \uAC12\
  \uC744 \uC0DD\uC131\uD558\uAE30 \uC704\uD574 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uD65C\uC6A9\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 무엇을, 왜?

Rust에서 난수 생성은 암호학, 시뮬레이션, 게임 및 무작위 알고리즘에 이르기까지 다양한 작업에 필수적인 예측할 수 없는 숫자 값을 생성하기 위해 라이브러리를 활용하는 것을 포함합니다.

## 방법:

Rust는 난수 생성을 위해 rand와 같이 가장 일반적으로 사용되는 외부 크레이트에 의존합니다. 난수를 생성하기 시작하려면 먼저 `Cargo.toml` 파일에 `rand`를 추가해야 합니다.

```toml
[dependencies]
rand = "0.8.5"
```

다음으로, Rust 코드에서 `rand`를 사용하여 난수를 생성할 수 있습니다. 여기에 정수와 부동 소수점 수를 생성하는 예제가 있습니다.

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // 1부터 10까지의 무작위 정수 생성
    let random_int: i32 = rng.gen_range(1..=10);
    println!("무작위 정수: {}", random_int);
    
    // 0.0부터 1.0 사이의 무작위 부동 소수점 수 생성
    let random_float: f64 = rng.gen::<f64>();
    println!("무작위 부동 소수점: {}", random_float);
}
```

샘플 출력은 다음과 같을 수 있습니다.

```plaintext
무작위 정수: 7
무작위 부동 소수점: 0.9401077112175732
```

프로그램을 다시 실행하면 다른 값을 생성한다는 것에 주의하세요.

## 심층 분석

`rand`와 그 의존성인 `getrandom`을 통해 촉진되는 Rust에서의 난수 생성은 운영 체제 기능과 알고리즘 생성기에 대한 광범위한 추상화를 나타냅니다. 역사적으로 컴퓨팅에서의 무작위성은 단순하고 예측 가능한 알고리즘에서부터 복잡하고 암호화된 방법론으로 발전했습니다. Rust의 접근 방식은 필요한 무작위성의 품질과 성능에 따라 다양한 생성기를 지원할 수 있는 플러그 가능한 `Rng` 트레잇을 통해 이러한 진화를 포함합니다.

대부분의 응용 프로그램의 경우, `rand`와 시스템의 RNG에 의존하는 것은 간단함과 엔트로피 사이의 좋은 균형을 제공합니다. 그러나 암호화 응용 프로그램의 경우, 크레이트는 시드를 위해 `getrandom`에 의존하며, 이는 OS 특정 메커니즘(예: 유닉스와 같은 시스템의 `/dev/urandom`)에 의존하여 암호학적으로 안전한 무작위성을 보장합니다.

대안적으로, `rand`가 충족시키지 못하는 특정 요구 사항이 있는 경우, 다른 크레이트를 탐색하거나 수학적 모델을 기반으로 한 맞춤형 생성기를 구현하는 것이 한 가지 방법일 수 있습니다. 그럼에도 불구하고, 대부분의 사용 사례에 대해, `rand`와 그 생태계는 효율적이면서도 Rust 응용 프로그램에 통합하기 쉬운 강력한 해결책을 제공합니다.
