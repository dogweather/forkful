---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체는 특정 문자열을 찾아 다른 문자열로 대체하는 것입니다. 이것은 데이터 정제, 자동화, 기타 다양한 태스크를 수행하기 위하여 프로그래머들이 자주 사용하는 방법입니다.

## 방법:

Rust에서 텍스트를 검색하고 교체하는 기본적인 방법을 알아보겠습니다.
```Rust
 fn main() {
    let before = "안녕하세요 Rust입니다!";
    let after = before.replace("Rust", "세계");
    println!("{}", after);
 }
```
이 코드가 실행되면 출력은 다음과 같습니다:

```Rust
"안녕하세요 세계입니다!"
```

위의 예제에서는 `replace` 함수를 사용하여 문자열 "Rust"를 "세계"로 교체하였습니다.

## 심화학습

### 역사적인 맥락
텍스트 검색 및 교체는 오래전부터 존재해 왔습니다. 이것은 프로그래밍의 중요한 요소로, 다양한 언어에서 그 방법과 구현이 달리하여 계속 발전해 왔습니다.

### 대안
Rust에서는 정규 표현식(Regex)을 이용하여 더 복잡한 문자열 검색 및 교체를 수행할 수 있습니다.

```Rust
use regex::Regex;
fn main() {
    let re = Regex::new("Rust").unwrap();
    let result = re.replace("안녕하세요 Rust입니다!", "세계");
    println!("{}", result);
}
```
### 구현 디테일
Rust의 .replace는 입력받은 문자열에서 모든 인스턴스를 찾아서 교체합니다.

## 추가정보

더 많은 정보를 찾아볼 수 있는 웹사이트 링크입니다:
- Rust 문서: Replace: [Rust Doc: Replace](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- Rust 정규 표현식: [Rust Regex](https://docs.rs/regex/)