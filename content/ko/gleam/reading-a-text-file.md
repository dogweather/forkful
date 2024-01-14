---
title:                "Gleam: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그램을 작성하거나 데이터를 분석하는 데 필요한 매우 기본적인 작업입니다. 이 기술을 배우면 더 나은 프로그래머가 될 수 있습니다.

## 어떻게

먼저, Gleam에서 도구를 로드해야 합니다. 그런 다음, 파일 경로와 함께 `gleam_text_file` 함수를 사용하여 텍스트 파일을 로드하고, 결과를 변수에 할당하십시오.

```Gleam
// 예시 파일 경로
let file_path = "example.txt"

// 파일 읽기
let file = gleam_text_file.load(file_path)
```

텍스트 파일을 읽은 후, 데이터를 사용해 원하는 방식으로 가공할 수 있습니다. 예를 들어, `string` 타입으로 값을 추출하거나 `interprete` 함수를 사용하여 문자열을 다른 타입으로 변환할 수 있습니다.

```Gleam
// 예시 파일에서 string 값 추출
let name = file.name

// string 값을 int 타입으로 변환
let age = file.age |> interprete.int
```

## 고수정보

텍스트 파일을 읽는 더 깊은 정보를 알고 싶다면, `gleam_text_file` 모듈의 공식 문서를 참조하십시오. 이 모듈에는 파일을 읽는 방법 외에도 파일의 존재 여부를 확인하고 쓰기 및 수정하는 방법도 포함되어 있습니다.

## 연관된 것들

- [Gleam 공식 문서](https://gleam.run/building-getting-started.html)
- [Gleam 텍스트 파일 모듈 문서](https://gleam.run/building-stdlib-modules.html#gleam-text-file)
- [다른 Gleam 모듈 살펴보기](https://gleam.run/building-stdlib-modules.html)