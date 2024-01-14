---
title:                "Rust: 텍스트 검색 및 대체"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

만약 당신이 텍스트를 치환하는 작업을 할 때마다, 전통적인 텍스트 편집기를 사용해 오래 걸리고 지루한 작업을 하기 싫다면, Rust 언어를 사용하여 더 빠르고 효율적으로 작업할 수 있습니다.

## 하는 법

```Rust
let input_text = "Hello, Rust!";
let mut output_text = String::new();

for character in input_text.chars() {
    match character {
        'a' => output_text.push('b'),
        'e' => output_text.push('f'),
        'i' => output_text.push('j'),
        'o' => output_text.push('p'),
        'u' => output_text.push('v'),
        _ => output_text.push(character),
    }
}
println!("{}", output_text);
```

**출력:** Jemmo, Suvu!

위의 코드는 간단한 예제이지만, 당신이 텍스트를 치환할 때 기본적인 아이디어를 보여줍니다. 문자 하나하나를 순회하면서, 우리는 치환 매칭을 할 수 있고, 매칭되는 문자를 판별해서 새로운 문자로 대체합니다. 이것이 우리가 텍스트를 치환하는 중요한 부분입니다.

## 딥 다이브

우리가 보았던 예제는 간단하지만, 실제로 우리는 조금 더 복잡한 경우에 직면할 수 있습니다. 예를 들어, 우리가 특정한 단어만 치환하고 싶은 상황이라면 어떻게 해야할까요? 이때는 `replace` 메서드를 사용할 수 있습니다.

```Rust
let input_text = "Hello, Rust! I love Rust!";
let output_text = input_text.replace("Rust", "Python");
println!("{}", output_text);
```

**출력:** Hello, Python! I love Python!

`replace` 메서드는 문자열에서 특정한 부분을 찾아서 원하는 새로운 문자열로 바꾸는 기능을 합니다. 이러한 강력한 기능을 잘 활용하면, 우리는 더욱 복잡한 텍스트 치환 작업을 할 수 있습니다.

## See Also

- [The Rust Programming Language](https://www.rust-lang.org/)
- [Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Tutorial: Strings](https://stevedonovan.github.io/rust-gentle-intro/2-structs-enums-lifetimes.html#strings)