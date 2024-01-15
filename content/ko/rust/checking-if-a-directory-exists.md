---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Rust: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
디렉토리가 존재하는지 확인하는 것이 왜 중요할까요? 일반적으로 디렉토리를 만들기 전에 먼저 디렉토리가 이미 존재하는지 확인하는 것은 중요합니다. 이를 확인하지 않으면 중복된 디렉토리를 만들 수 있고, 이로 인해 데이터가 잃어지거나 파일 시스템이 오동작할 수 있습니다.

## 사용 방법
디렉토리가 존재하는지 확인하는 방법은 간단합니다. 우선, 표준 라이브러리에서 제공하는 `std::path::Path` 타입을 사용합니다. 이 타입은 파일 시스템 경로를 나타내는데 사용됩니다. 그리고 `exists()` 메서드를 호출하여 디렉토리의 존재 여부를 확인할 수 있습니다. 아래는 예시 코드와 실행 결과입니다.

```Rust
use std::path::Path;

let path = Path::new("./example_directory");
if path.exists() {
    println!("This directory exists!");
} else {
    println!("This directory does not exist.");
}
```
```
실행 결과:
This directory does not exist.
```
주의할 점은, `exists()` 메서드는 디렉토리와 파일의 존재 여부를 모두 확인할 수 있다는 것입니다.

## 딥 다이브
`exists()` 메서드의 구현은 플랫폼마다 다릅니다. 리눅스의 경우, `access()` 시스템 콜을 사용하여 디렉토리의 존재 여부를 확인합니다. 반면 윈도우의 경우, `GetFileAttributesW()` 함수를 사용합니다. 이런 차이점으로 인해 디렉토리의 존재 여부를 확인하는 성능도 플렛폼마다 다를 수 있습니다. 그래서 만약 디렉토리의 존재 여부를 빠르게 확인해야 한다면, 여러 플랫폼에서 빠르게 동작하는 코드를 작성해야 합니다.

## 참고 자료
- [Rust 표준 라이브러리 문서](https://doc.rust-lang.org/std/fs/struct.Permissions.html#method.set_readonly)
- [리눅스 `access()` 시스템 콜 문서](https://man7.org/linux/man-pages/man2/access.2.html)
- [윈도우 `GetFileAttributesW()` 함수 문서](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesw)

## 참고
[Rust 표준 라이브러리 문서](https://doc.rust-lang.org/std/index.html)에서 더 많은 정보를 찾아볼 수 있습니다.