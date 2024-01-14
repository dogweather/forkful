---
title:                "Gleam: 텍스트 파일 읽기"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 포스트 - 한국 독자들을 위해

# 왜

## 왜 읽기

텍스트 파일을 읽는 이유는 주로 데이터를 처리하거나 분석하기 위해서입니다. 읽은 데이터를 바탕으로 프로그래밍 작업을 수행하거나 파일 내용을 확인하는 등 다양한 목적으로 사용될 수 있습니다.

## 어떻게

Gleam에서 텍스트 파일을 읽는 가장 기본적인 방법은 `File.read` 함수를 사용하는 것입니다. 이 함수는 파일의 경로를 인자로 받아서 파일의 내용을 읽어올 수 있습니다.

```Gleam
let path = "file.txt"
let content = File.read(path)
```

위 예시는 `file.txt`라는 파일을 읽어서 해당 파일의 내용을 `content` 변수에 저장합니다. 파일의 내용은 문자열 형태로 저장됩니다.

## 깊이 파고들기

파일을 읽는 데 있어서 유의해야 할 점은 파일이 존재하지 않거나 잘못된 경로를 입력하면 오류가 발생한다는 것입니다. 이러한 상황에 대비해서 `File.read` 함수를 사용할 때는 `Option` 타입을 사용하는 것이 좋습니다.

예를 들어서 아래와 같이 `match` 표현을 사용하여 파일을 읽는 작업을 수행할 수 있습니다.

```Gleam
let path = "file.txt"
let result = match File.read(path) {
    Ok(content) -> content
    Error(_) -> "파일을 읽는 중 오류가 발생했습니다."
}
```

위 예시에서 `match` 표현을 사용하면 `File.read` 함수가 `Ok` 값을 반환하면 해당 값을 `result` 변수에 저장하고, `Error` 값을 반환하면 오류 메시지를 출력하는 것을 볼 수 있습니다.

# 관련 항목

- [Gleam 공식 문서 - 파일 입출력](https://gleam.run/documentation/stdlib/file.html)
- [Gleam Community Forum - 파일 관련 질문들과 답변들](https://gleam.discourse.group/search?q=file)