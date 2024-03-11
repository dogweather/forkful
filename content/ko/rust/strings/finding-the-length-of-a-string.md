---
date: 2024-01-20 17:48:23.919459-07:00
description: "\uBB38\uC790\uC5F4 \uAE38\uC774 \uCC3E\uAE30\uB294 \uBB38\uC790\uC758\
  \ \uCD1D \uAC1C\uC218\uB97C \uC138\uB294 \uAC83\uC785\uB2C8\uB2E4. \uBA54\uBAA8\uB9AC\
  \ \uC0AC\uC6A9\uC744 \uCD5C\uC801\uD654\uD558\uAC70\uB098, \uD2B9\uC815 \uAE38\uC774\
  \uC758 \uC785\uB825\uC744 \uD544\uC694\uB85C \uD560 \uB54C \uC720\uC6A9\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:28.822631-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uAE38\uC774 \uCC3E\uAE30\uB294 \uBB38\uC790\uC758 \uCD1D\
  \ \uAC1C\uC218\uB97C \uC138\uB294 \uAC83\uC785\uB2C8\uB2E4. \uBA54\uBAA8\uB9AC \uC0AC\
  \uC6A9\uC744 \uCD5C\uC801\uD654\uD558\uAC70\uB098, \uD2B9\uC815 \uAE38\uC774\uC758\
  \ \uC785\uB825\uC744 \uD544\uC694\uB85C \uD560 \uB54C \uC720\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열 길이 찾기는 문자의 총 개수를 세는 것입니다. 메모리 사용을 최적화하거나, 특정 길이의 입력을 필요로 할 때 유용합니다.

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
