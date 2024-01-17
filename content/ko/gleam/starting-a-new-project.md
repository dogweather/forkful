---
title:                "새 프로젝트 시작하기"
html_title:           "Gleam: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

새로운 프로젝트를 시작한다는 것은 다른 말로 처음부터 코딩을 시작한다는 것을 의미합니다. 프로그래머들은 새로운 프로젝트를 시작하는 이유는 여러 가지가 있지만, 주로 새로운 기술을 배우거나 특정한 목적을 위해 새로운 소프트웨어를 작성하기 위해서입니다. 

## 방법:

```Gleam ... ``` 코드 블록 안에 코딩 예제와 출력 예시를 포함하여 설명하겠습니다.

```Gleam
import gleam/io

pub fn main() {
  io.print("Hello, World!")
}
```

위의 코드는 "Hello, World!"를 출력하는 기본적인 새로운 프로젝트를 시작하는 방법을 보여줍니다. 코드를 작성하고 다음과 같이 Gleam 인터프리터로 실행할 수 있습니다.

```
> gleam run main.gleam
Hello, World!
```

## 깊이 파고들기:

새로운 프로젝트를 시작하는 것은 코드 작성의 시작에 불과하지만, 이 자체로도 중요한 결정을 한 것입니다. 예를 들어, 어떤 언어를 사용할지, 어떤 도구를 사용할지, 어떤 디자인 패턴을 따를지 등을 고민해야 합니다. 또한 새로운 프로젝트를 시작할 때 다른 언어나 도구를 사용할 수도 있으며, 많은 선택지가 있을 수 있습니다. Gleam 외에도 Erlang 같은 함수형 언어나 Rust 같은 시스템 프로그래밍 언어를 사용할 수도 있습니다. 새로운 프로젝트를 시작할 때마다 새로운 언어나 도구를 배우는 것은 매우 유익한 과정이 될 수 있습니다.

## 관련 자료:

- [Gleam 공식 문서](https://gleam.run)
- [Gleam GitHub 저장소](https://github.com/gleam-lang/gleam)
- [Gleam 커뮤니티 포럼](https://community.gleam.run/)