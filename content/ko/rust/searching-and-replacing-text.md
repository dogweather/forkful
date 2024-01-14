---
title:    "Rust: 텍스트 검색과 바꾸기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 것에 관심이 있는 이유는 다양할 수 있습니다. 예를 들어, 여러 줄의 코드에서 일부 특정 값을 변경하고 싶거나, 메모장이나 워드 문서에서 특정 단어를 모두 찾아 바꾸고 싶을 때 사용할 수 있습니다. 언제든 필요한 경우, 텍스트를 쉽고 효율적으로 변경할 수 있도록 할 수 있습니다.

## 하는 방법

우선, Rust 언어를 사용하여 텍스트를 검색하고 바꾸는 기본적인 방법을 알아보겠습니다. 다음의 코드 블럭을 참고해주세요.

```Rust
use std::fs;
use regex::Regex;

let contents = fs::read_to_string("sample.txt")
    .expect("파일을 읽을 수 없습니다.");

let re = Regex::new(r"Hello|world").unwrap();
let result = re.replace_all(&contents, "안녕하세요");
println!("{}", result);
```

위 코드는 전체 파일에서 "Hello" 또는 "world"라는 단어를 찾아 "안녕하세요"로 바꾸는 간단한 예제입니다.

위의 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
안녕하세요, 안녕하세요. 오늘은 안녕하세요.
```

위의 예제는 Rust 표준 라이브러리와 `regex` 라이브러리를 사용하여 텍스트를 검색하고 바꾸는 방법을 보여줍니다. `regex`는 정규식을 사용하여 더 복잡한 규칙을 적용할 수 있도록 도와줍니다.

## 깊게 파고들기

검색하고 바꾸는 기능에 대해 더 많은 정보를 알고 싶은 경우에는 공식 문서를 참조하는 것이 가장 좋습니다. 또는 Rust 사용자 커뮤니티에서 다른 개발자들의 팁과 트릭을 공유하는 게시글을 찾아볼 수도 있습니다.

## 또한 참고해주세요

- [Rust 공식 문서](https://www.rust-lang.org/learn)
- [Rust 커뮤니티 포럼](https://users.rust-lang.org/)