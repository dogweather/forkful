---
title:    "Rust: 랜덤 숫자 생성"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤 숫자를 생성하는 것에 참여하는 이유는 데이터 분석, 게임 프로그래밍 및 보안 시스템에서 다양한 목적으로 사용될 수 있기 때문입니다.

## 방법

```Rust
use rand::Rng;

fn main() {
  // 1부터 10까지의 랜덤한 숫자 출력
  let num = rand::thread_rng().gen_range(1..=10);
  println!("{}", num);
}
```
출력 예시:
5


## 깊이 파고들기

랜덤 숫자 생성에는 여러 가지 방식이 있습니다. 그 중에서도 주로 사용되는 데 랜덤 라이브러리를 사용하여 보다 안전하고 무작위한 숫자를 생성할 수 있습니다. 또한 초기 시드 값을 설정하는 등 추가적인 설정도 가능합니다.

## 더 알아보기

- [Rust에서 랜덤 숫자 생성하기](https://doc.rust-lang.org/stable/book/ch07-03-importing-names-with-use.html#using-external-packages)
- [Rust의 rand 라이브러리](https://docs.rs/rand/)