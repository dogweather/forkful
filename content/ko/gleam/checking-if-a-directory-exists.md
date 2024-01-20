---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

디렉토리가 존재하는지 확인하는 것은 파일시스템에서 특정 경로가 이미 존재하는지 여부를 검사하는 것을 뜻합니다. 프로그래머들은 이를 통해 파일 작성 또는 디렉토리 수정 전에 오류를 방지할 수 있습니다.

## 방법:

Gleam에서는 'gleam/erlang' 라이브러리를 사용하여 어떤 디렉토리가 존재하는지 확인합니다.

```Gleam
import gleam/erlang

fn is_directory_exists(directory: String) -> Bool {
  erlang.fs_element_type(directory) == Ok(#{"type": erlang.Directory})
}

fn main() {
  let directory = "your/directory/path"
  let result = is_directory_exists(directory)
  
  case result {
    True -> erlang.print("디렉토리가 존재합니다")
    False -> erlang.print("디렉토리가 존재하지 않습니다")
  }
}
```

## 깊게 알아보기

디렉토리의 존재 여부를 확인하는 것은 사실상 모든 파일 시스템 작업의 시작점입니다. 또한, 한 소프트웨어가 다른 소프트웨어와 효과적으로 통신하기 위해 파일을 사용하는 많은 레거시 시스템에서 중요합니다.

이와 같은 기능의 대안으로는 `filelib:is_directory/1`과 같은 다른 Erlang 함수를 사용하는 것이 있지만, Gleam의 타입 안전성 때문에 이 함수를 사용하는 것이 더 바람직하며, 에러를 방지할 수 있습니다.

## 관련 정보:

다음 링크를 통해 추가 정보를 얻을 수 있습니다: 

[Gleam documentation](https://hexdocs.pm/gleam_erlang/gleam/erlang/index.html)  
[Erlang's filelib documentation](http://erlang.org/doc/man/filelib.html)