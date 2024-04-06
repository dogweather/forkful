---
date: 2024-01-20 17:48:23.919459-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Rust\uC5D0\uC11C \uBB38\
  \uC790\uC5F4 \uAE38\uC774\uB97C \uCC3E\uB294 \uBC95\uC740 \uAC04\uB2E8\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.696820-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Rust\uC5D0\uC11C \uBB38\uC790\uC5F4\
  \ \uAE38\uC774\uB97C \uCC3E\uB294 \uBC95\uC740 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (어떻게 하나요?)
Rust에서 문자열 길이를 찾는 법은 간단합니다.

```Rust
fn main() {
    let message = "안녕하세요!";
    let length = message.chars().count();
    
    println!("Length: {}", length);
}
```

출력:
```
Length: 6
```

## Deep Dive (심층 분석)
문자열 길이를 측정하는 것은 아주 오래전부터 있었습니다. Rust에서는 `.len()` 메서드를 사용할 때 주의해야 합니다. 왜냐하면 UTF-8에 기반한 문자열에서 바이트 수를 반환하기 때문입니다.

```Rust
let message = "안녕하세요!";
println!("Length using .len(): {}", message.len());
```

출력:
```
Length using .len(): 15
```

`.chars().count()`는 가장 정확한 문자 수를 세는 방법입니다. `.len()`은 바이트 크기를 반환하므로 유니코드 문자에는 적합하지 않습니다.

## See Also (추가 정보)
- Rust 공식 문서 문자열 섹션: [Rust String Docs](https://doc.rust-lang.org/stable/std/string/)
- UTF-8 인코딩에 대한 더 많은 정보: [Understanding UTF-8](http://www.fileformat.info/info/unicode/utf8.htm)
- Rust 프로그래밍에 대한 더 나은 이해를 위한 The Rust Book: [The Rust Programming Language](https://doc.rust-lang.org/book/)
