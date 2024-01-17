---
title:                "랜덤 숫자 생성하기"
html_title:           "Rust: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

우리가 컴퓨터 프로그래밍을 할 때, 우리가 사용하는 많은 프로그램에서 난수(random numbers)를 사용하는 것을 볼 수 있을 것입니다. 난수는 컴퓨터가 생성할 수 있는 임의의 숫자를 의미합니다. 프로그래머들은 이러한 난수를 사용하여 다양한 작업을 수행하며, 예측할 수 없는 결과를 만들 수 있습니다.

## 어떻게:

```Rust
use rand::{Rng, thread_rng};

fn main() {
    // 0부터 100사이의 난수 생성
    let mut rng = thread_rng();
    let random_number: u32 = rng.gen_range(0, 100);
    println!("Random number is: {}", random_number);
}
```
출력:
```
Random number is: 74
```

위의 예시 코드는 ```rand``` 라이브러리를 사용하여 0부터 100 사이의 난수를 생성하는 방법을 보여줍니다. 로컬의 다른 난수 생성기를 사용할 수도 있습니다.

## 깊이 들어가기:

우리가 현재 사용하는 컴퓨터는 난수를 생성하는 방법에 대해 매우 발전했습니다. 과거에는 사용자의 입력이나 장치의 하드웨어 물리적 변화를 사용하여 난수를 생성했지만, 현재는 암호학적으로 안전한 방식의 난수 생성기가 개발되었습니다. 또한 다른 난수 생성 방법으로는 유사 난수 생성기 또는 암호 해시 함수를 사용할 수 있습니다.

## 참고 자료:

난수 생성에 대한 더 자세한 내용은 다음 링크를 참고하시기 바랍니다:
- 랜덤넘버(Random number) - https://ko.wikipedia.org/wiki/%EB%9E%9C%EB%8D%A4%EB%84%98%EB%B2%84
- 난수(Randomness) - https://ko.wikipedia.org/wiki/%EB%82%9C%EC%88%98