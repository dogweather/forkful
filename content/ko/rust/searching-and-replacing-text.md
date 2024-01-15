---
title:                "텍스트 검색 및 치환하기"
html_title:           "Rust: 텍스트 검색 및 치환하기"
simple_title:         "텍스트 검색 및 치환하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
명확하고 간결한 코드 작성은 모든 프로그래머에게 중요합니다. 그리고 때로는 우리는 텍스트 내에서 특정 패턴을 찾아 다른 패턴으로 바꿔야 할 때가 있습니다. 이를테면, 파일 이름의 형식을 통일하거나 일괄적으로 단어를 변경하는 등의 상황에서 우리는 검색 및 대체 기능이 필요합니다.

## 하우 투
```Rust
use std::fs::File;
use std::io::prelude::*;

// 파일 이름 형식을 통일하기 위해 "filename"을 "FILENAME"으로 변경합니다.
fn replace_filename() {
    let mut file = File::open("some_file.txt").expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");
    let new_contents = contents.replace("filename", "FILENAME");
    println!("{}", new_contents);
}
```

위 코드는 `some_file.txt` 파일을 열고 그 내용을 읽은 후, `filename`을 `FILENAME`으로 대체한 결과를 출력합니다.

## 딥 다이브
Rust의 `String` 타입에는 `replace()` 메서드가 있습니다. 이 메서드를 사용하면 텍스트 내의 모든 해당 패턴을 찾아 바꿀 수 있습니다. 또한, `replace()` 메서드는 주어진 텍스트가 없을 때는 아무것도 변경하지 않으므로 안전하게 사용할 수 있습니다. 따라서 우리는 검색 및 대체 작업을 위해 `replace()` 메서드를 유용하게 사용할 수 있습니다.

## See Also
- [Rust 공식 문서](https://doc.rust-lang.org/std/string/struct.String.html#method.replace) 
- [Rust Cookbook: Searching and Replacing Text](https://rust-lang-nursery.github.io/rust-cookbook/text/finding.html#search-and-replace) 
- [Replace text in files with Rust](https://medium.com/@rjmm/rust-for-replacing-text-in-files-it-is-surprisingly-easy-ba343e6c4410)