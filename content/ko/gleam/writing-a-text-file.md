---
title:                "Gleam: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 작성하는 것에 참여하는 이유는 무엇일까요? 그것은 우리의 코드 및 정보를 저장하고 유지하기 위한 가장 기본적인 방법 중 하나입니다. 따라서 당신이 프로그래머라면, 텍스트 파일 작성을 배우는 것은 굉장히 중요합니다.

## 하우 투

Gleam에서 텍스트 파일을 작성하기 위해서는 `File.write` 함수를 사용합니다. 예를 들어, 다음의 코드를 참고하세요.

```Gleam
import gleam/file

let data = "Hello world!"
let result = File.write("./test.txt", data)
```

위의 예시에서, `data` 변수에는 "Hello world!"라는 문자열이 저장되어 있으며, 이를 `File.write` 함수를 사용하여 "test.txt"라는 파일에 쓰게 됩니다. 만약 파일이 성공적으로 작성되었다면, `result` 변수에는 `{:ok, ()}`라는 값을 갖게 됩니다. 이제 "test.txt" 파일을 열어보면 "Hello world!"라는 내용이 쓰여져 있는 것을 확인할 수 있습니다.

## 딥 다이브

텍스트 파일을 작성하는 데에는 더 많은 것들이 있습니다. 예를 들어, `File.open` 함수를 사용하면 이미 존재하는 파일에 데이터를 추가할 수 있습니다. 또한, `File.delete` 함수를 사용하여 파일을 삭제할 수도 있습니다.

또한 텍스트 파일을 읽고 쓰는 데에는 다양한 형식이 있을 수 있습니다. 예를 들어 CSV 파일을 읽고 쓰는 방법을 배우는 것도 중요합니다. 따라서 더 깊게 알아보고 싶다면 Gleam 공식 문서를 참고하는 것을 추천합니다.

## 참고 자료

- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Gleam 텍스트 파일 관련 문서](https://gleam.run/documentation/syntax/text_files)
- [파일 작업 함수 예제](https://github.com/gleam-lang/gleam_stdlib/blob/master/gleam-file/test/file.test.gleam)