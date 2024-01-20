---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:56:25.048496-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

디렉토리가 존재하는지 확인하는 것은 파일 시스템에서 특정 경로에 폴더가 있는지 여부를 검사하는 과정입니다. 프로그래머들은 리소스가 잘못된 위치에 접근하지 않도록, 혹은 동일한 이름의 폴더가 이미 있을 때 중복 생성을 피하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)

```gleam
import gleam/io
import gleam/result

pub fn check_directory_exists(path: String) -> result.Result(Nil, OSError) {
  io.is_dir(path)
}

// 사용 예:
case check_directory_exists("path/to/directory") {
  Ok(_) -> "Directory exists!"
  Error(_) -> "Directory does not exist."
}
```
Sample output:
```
"Directory exists!"
```
Or if the directory doesn't exist:
```
"Directory does not exist."
```

## Deep Dive (깊은 탐구)

디렉플리 확인 기능이 필요한 이유는 파일 시스템 관리에 있어 오류를 방지하고, 안정성을 높이기 위해서입니다. 역사적으로, 이러한 확인은 파일 IO 작업 전에 수행하는 보편적인 사전 조건입니다. Gleam은 에를랑(Erlang)의 강인성과 타입 안전성을 결합하여 이러한 작업을 보다 쉽고 명확하게 해줍니다. 대안으로 파일 또는 디렉토리를 직접 열려고 시도하고, 실패하면 그것이 존재하지 않는다는 결론을 내릴 수도 있습니다. 하지만 이는 예외 처리가 필요하고, Gleam의 타입-안전 접근 방식과는 다소 거리가 있습니다.

## See Also (함께 보기)

- Gleam 파일 시스템 관련 작업: [gleam_stdlib](https://hex.pm/packages/gleam_stdlib)
- 에를랑 OS 파일 모듈: [Erlang :file module](http://erlang.org/doc/man/file.html)