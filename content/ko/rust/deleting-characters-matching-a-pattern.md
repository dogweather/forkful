---
title:                "패턴에 일치하는 문자 삭제하기"
html_title:           "Rust: 패턴에 일치하는 문자 삭제하기"
simple_title:         "패턴에 일치하는 문자 삭제하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자가 패턴과 일치하는 것을 삭제한다는 것은 무엇을 의미할까요? 프로그래머들이 이를 하는 이유는 무엇일까요? 

이 기능은 문자열에서 특정한 패턴에 매칭되는 문자들을 삭제하는 것을 의미합니다. 보통, 프로그래머들은 이 기능을 사용해 특정한 문자를 필터링하거나 삭제함으로써 문서나 데이터를 정리하고 다루기 쉽게 만듭니다.

## 방법:
아래의 ```Rust ... ``` 코드 블록을 사용해 예제 코드와 출력 결과를 확인해보세요.

```Rust
let input_string = String::from("Hello, World!");
let output_string = input_string.replace("l", "");
println!("{}", output_string);

// Output: Heo, Word!
```

## 깊이 파고들기:
문자 삭제 기능은 프로그래밍 언어에서 오랜 시간 동안 사용되어왔습니다. 예전에는 문자를 삭제하거나 수정하기 위해 자주 사용되는 방법이었지만, 오늘날에는 보편적으로 정규표현식을 사용하게 되었습니다.

또한, 더 나은 성능과 다양한 기능을 제공하는 여러 다른 라이브러리들도 있습니다. 예를 들어, ```regex``` 라이브러리는 정규표현식을 조작하는 더 강력한 방법을 제공합니다.

## 관련 자료:
- Rust 언어 공식 홈페이지: https://www.rust-lang.org
- ```regex``` 라이브러리 공식 문서: https://docs.rs/regex/latest/regex/