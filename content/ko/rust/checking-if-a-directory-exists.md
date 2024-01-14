---
title:                "Rust: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜

누군가 디렉토리가 있는지 확인하는 것에 관여하려는 이유는 무엇일까요?

디렉토리를 만들기 전에 먼저 디렉토리가 존재하는지 확인하는 것은 매우 중요합니다. 이는 이미 디렉토리가 있다면 중복되는 디렉토리를 만들지 않을 수 있기 때문입니다. 또한 디렉토리가 없다면 새로운 디렉토리를 만들 수 있는지 확인할 수 있습니다.

# 어떻게 의

여러분은 Rust로 디렉토리가 존재하는지 확인하는 방법을 알고 계시나요? 그렇지 않다면 걱정하지 마세요. 이번 글에서는 이를 쉽게 할 수 있는 방법을 알려드리겠습니다.

먼저, 여러분은 `std::fs` 모듈에서 제공하는 `metadata` 메서드를 사용하면 됩니다. 이 메서드는 주어진 경로에 대한 메타데이터를 반환합니다. 따라서, 디렉토리가 존재하는지 확인하려는 경우 해당 디렉토리의 경로를 매개변수로 넘겨주면 됩니다.

아래는 이를 구현한 간단한 예제 코드입니다.

```Rust
use std::fs;

fn main() {
    let path = "path/to/directory";
    let exists = fs::metadata(path).is_ok();
    
    if exists {
        println!("Directory exists!");
    } else {
        println!("Directory does not exist!");
    }
}
```

위 코드에서 주목해야 할 부분은 `metadata` 메서드를 호출한 뒤 `is_ok` 메서드를 호출하여 결과를 확인하는 부분입니다.

# 깊이 파고들기

보다 정확하게 디렉토리가 존재하는지 확인해보겠습니다. `metadata` 메서드는 `Result` 타입을 반환하기 때문에, 이는 성공한 경우 `Ok` 값과 실패한 경우 `Err` 값을 반환합니다. 따라서, 아래와 같이 `match` 표현식을 사용하여 각각의 경우에 대해 처리할 수 있습니다.

```Rust
use std::fs;

fn main() {
    let path = "path/to/directory";
    let result = fs::metadata(path);
    
    match result {
        Ok(_) => println!("Directory exists!"),
        Err(_) => println!("Directory does not exist!"),
    }
}
```

만약 디렉토리가 존재한다면, 반환된 메타데이터에서 여러 정보들을 얻을 수 있습니다. 예를 들어, 디렉토리의 수정 시간을 얻고 싶다면 아래와 같이 코드를 작성할 수 있습니다.

```Rust
use std::fs;

fn main() {
    let path = "path/to/directory";
    let result = fs::metadata(path);
    
    match result {
        Ok(meta) => {
            let modified = meta.modified()?;
            println!("Directory modified at: {:?}", modified);
        },
        Err(_) => println!("Directory does not exist!"),
    }
}
```

여러분은 `?` 연산자를 사용하여 `metadata` 호출의 결과를 반환하는 동시에 `Err` 값을 처리할 수 있습니다. 마찬가지로, 여러분은 `st_mode` 필드를 사용하여 파일의 권한을 확인할 수도 있습니다.

# 그 외에도

위에서는 디렉토리가 있는지 확인하는 방법에 대해 알아보았습니다. 자세한 정보는 Rust 공식 문서에서 확인하실 수 있습니다.

## 더 알아보기

- [Rust 공식 문서 - 파일 시스템](https://doc.rust-lang.org/std/fs/)
- [Rust by Example - 파일 시