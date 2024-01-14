---
title:                "Rust: 난수 생성하기"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜?
랜덤 숫자를 생성하는 것에 대해 생각해보았을 때, "왜 필요한가?"라는 의문이 드는 분들도 있으실 것입니다. 하지만 랜덤 숫자는 많은 분야에서 필수적으로 사용되는 기능입니다. 예를 들어 게임에서는 랜덤으로 아이템을 드롭하고, 보안 알고리즘에서는 암호를 생성하기 위해 랜덤 숫자를 사용합니다. 랜덤 숫자를 생성하는 것은 우리의 일상에서도 매우 자주 사용되는 기능입니다.

# 방법
랜덤 숫자를 생성하기 위해 Rust 언어의 랜덤 라이브러리를 사용할 수 있습니다. 먼저, `rand` 라이브러리를 제공하는 [crates.io](https://crates.io/) 웹사이트에서 라이브러리를 다운로드합니다. 다음으로, 우리가 원하는 랜덤 숫자의 범위를 설정할 수 있습니다. 예를 들어, `1`부터 `10` 사이의 랜덤 숫자를 생성하려면 다음과 같은 코드를 사용할 수 있습니다.

```Rust
use rand::Rng;

let mut rng = rand::thread_rng();
let number = rng.gen_range(1, 11);

println!("랜덤 숫자: {}", number);
```

위 코드에서 `rng` 변수는 `thread_rng()` 함수를 사용하여 스레드 로컬 랜덤 생성기를 생성합니다. 그리고 `gen_range()` 함수를 사용하여 원하는 범위의 랜덤 숫자를 생성합니다. 이처럼 간단한 코드로 우리는 원하는 범위의 랜덤 숫자를 쉽게 생성할 수 있습니다.

# 더 깊게
실제로 랜덤 숫자를 생성하는 것은 컴퓨터에서 가상적인 랜덤을 만드는 것입니다. 컴퓨터는 랜덤이라는 개념을 이해하지 못하기 때문에 특정한 알고리즘을 통해 랜덤 같은 숫자를 생성할 뿐입니다. 따라서 여러분은 적절한 랜덤 숫자의 범위와 라이브러리의 사용법을 알아야 합니다. 더 중요한 것은 랜덤 숫자를 사용하는 분야에 대해 잘 이해하는 것입니다. 이를 통해 랜덤 숫자를 보안에 활용할 수도 있고, 더 효율적으로 코드를 작성할 수도 있습니다.

# 더 알아보기
이 글에서 설명한 `rand` 라이브러리 외에도 랜덤 숫자를 생성하는 다양한 라이브러리가 있습니다. 그리고 랜덤 숫자를 사용하는 다양한 분야에 대해 더 알아 볼 수도 있습니다. 아래 링크에서 추가 정보를 찾아보세요.

* [Rust 공식 문서](https://doc.rust-lang.org/rand/rand/index.html)
* [랜덤 숫자 기능이 필요한 프로젝트 예제](https://github.com/rust-random/rand#examples)
* [랜덤 숫자를 사용하는 암호화 방식](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)

# 더 읽어보기