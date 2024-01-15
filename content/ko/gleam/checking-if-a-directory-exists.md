---
title:                "디렉토리 존재 여부 확인하기"
html_title:           "Gleam: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

이 글을 읽고 있는 당신은 아마도 Gleam 프로그래밍 언어를 사용하여 디렉토리가 존재하는지 확인하는 방법을 알고자 하는 분일 것입니다. 이 기술은 파일 경로를 다루는 프로그램을 작성할 때 매우 유용합니다.

## 방법

아래 Gleam 코드 블록 내부에서 코딩 예제와 예상 출력을 확인하세요.

```Gleam
import gleam/path

// 디렉토리 경로를 지정합니다.
let directory = path.join("documents", "programming", "gleam")

// 디렉토리가 존재하는지 확인합니다.
let exists = path.exists(directory)

// 존재하는 경우에는 메시지를 출력합니다.
if exists {
    io.print("디렉토리가 존재합니다!")
} else {
    io.print("디렉토리가 존재하지 않습니다.")
}
```

예상 출력:

`디렉토리가 존재합니다!`

## 딥 다이브

Gleam의 `path.exists` 함수는 파일이나 디렉토리가 실제로 존재하는지 여부를 확인하는 데 사용됩니다. 이 함수는 파일 시스템의 작업을 수행하기 전에 사전 조건으로서 사용될 수 있습니다. 또한 다른 Gleam 함수와 조합하여 보다 복잡한 파일 작업을 수행할 수도 있습니다.

## 또 다른 참고 자료

- [Gleam 공식 문서](https://gleam.run/) - Gleam 언어에 대한 상세한 정보와 사용법을 확인하세요.
- [Gleam 코드 샘플](https://github.com/gleam-lang/gleam/tree/master/examples) - Gleam 언어의 다양한 예제 코드를 살펴보세요.
- [Gleam 커뮤니티 포럼](https://elixirforum.com/c/gleam) - Gleam 언어 및 관련 기술에 대해 논의할 수 있는 포럼입니다.