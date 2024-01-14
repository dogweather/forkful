---
title:                "Rust: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜?

새로운 프로젝트를 시작하는 이유는 무엇일까요? 물론, 생각보다 굉장히 많은 이유가 있을 수 있지만요! 예를 들어, 배우고 싶은 새로운 언어, 새로운 도구를 사용해보고 싶은 욕망, 더 좋은 성능을 위한 차별점, 더 나은 코드 구조 등이 있을 수 있습니다. 어떤 이유든, 새로운 프로젝트를 시작하는 것은 새로운 도전과 자신감의 기회가 될 수 있습니다.

## 어떻게 하는가?

Rust는 언어 자체의 특징상 코드가 더욱 명확하고 간결해집니다. 그렇기 때문에 새로운 프로젝트를 시작하는 것도 빠르고 쉽습니다. 아래는 실제 예시를 보여드릴게요.

```Rust
fn main() {
    println!("Hello, world!");
}
```

위 코드를 실행하면 "Hello, world!"라는 메시지가 출력됩니다. 이는 이제 우리가 Rust를 실험할 준비가 끝난 것입니다. 이외에도 더 다양한 예제들을 살펴보며 차근차근 실습해보세요.

## 딥 다이브

새로운 프로젝트를 시작할 때, 꼭 먼저 해야할 일이 있습니다. 바로 "의존성(dependency) 목록"을 작성하는 것입니다. 이를 통해 추가로 필요한 외부 라이브러리들을 사용할 수 있게 되어 모듈화가 가능해집니다. 이때, cargo라는 Rust의 패키지 매니저를 사용해볼 수도 있습니다. 이제부터는 직접 프로젝트를 만들어보고 테스트를 진행해보세요!

## See Also

- The Rust Programming Language: https://www.rust-lang.org/
- Rust By Example: https://doc.rust-lang.org/rust-by-example/
- The Cargo Book: https://doc.rust-lang.org/cargo/