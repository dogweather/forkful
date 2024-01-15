---
title:                "랜덤 숫자 생성"
html_title:           "Rust: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

무작위 숫자를 생성하는 것에 참여해야 하는 *왜*에 대해 최대 두 문장으로 설명합니다.

무작위 숫자는 다양한 프로그래밍 시나리오에서 매우 유용합니다. 게임에서 적들의 위치를 결정하거나 보안 테스트를 위해 패스워드를 생성하는 등의 활용 방법이 있습니다. 이러한 이유들로 인해 무작위 숫자를 생성하는 것은 프로그래머에게 매우 중요한 기술입니다.

## How To

아래 코드 블록에서 ```Rust ... ``` 예시를 참고하여 무작위 숫자를 생성하는 방법을 살펴보세요.

```Rust
use rand::Rng;

// 1부터 10 사이의 무작위 정수를 생성합니다.
let random_num: u32 = rand::thread_rng().gen_range(1, 11);
println!("Generated number is: {}", random_num);

// 0부터 9 사이의 무작위 실수를 생성합니다.
let random_float: f64 = rand::thread_rng().gen_range(0.0, 10.0);
println!("Generated float is: {}", random_float);
```

위 코드에서 ```rand``` 크레이트의 ```gen_range``` 메서드를 사용하여 원하는 범위의 무작위 숫자를 생성할 수 있습니다. 또한 ```thread_rng()``` 함수를 사용하여 프로그램의 현재 스레드에 대한 무작위 생성기를 초기화해야 합니다.

## Deep Dive

추가적인 정보를 알고 싶다면 무작위 숫자 생성의 내부 동작 원리를 살펴보세요. 랜덤 값 생성은 난수 생성기의 결과를 기반으로 합니다. 난수 생성기는 시드(Seed)라고 부르는 입력 값을 이용하여 값을 생성합니다. 시드 값은 매우 중요한 역할을 하며 동일한 시드 값으로 난수 생성기를 초기화하면 항상 같은 숫자가 생성됩니다. 하지만 보통은 시간 값을 사용하여 시드 값을 계속 변경하면서 무작위한 숫자들을 생성합니다.

## See Also

- 랜덤 크레이트: https://docs.rs/rand/0.8.3/rand/
- 난수 생성기에 대한 더 자세한 정보: https://doc.rust-lang.org/std/rand/ 
- 랜덤 숫자 생성에 관련된 다른 기술: https://www.geeksforgeeks.org/using-rand-and-srand-in-programming-languages/