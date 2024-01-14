---
title:    "Gleam: 텍스트 파일 읽기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜

만약 당신이 Gleam 프로그래밍에 대해 궁금하다면, 파일을 읽는 것은 중요한 기술입니다. 이 기술은 텍스트 파일에서 데이터를 추출하는 데에 유용하며, 다양한 프로그래밍 프로젝트에 필요합니다. 이 글은 파일을 읽는 기본적인 방법과 Gleam 언어에서 다루는 법에 대해 알려드리겠습니다.

## 지금까지

우선 텍스트 파일을 읽는 방법을 보여드리기 위해, 사용자의 컴퓨터에 존재하는 파일을 읽어와보겠습니다. 그러기 위해서는 `File` 모듈을 불러와야 합니다. 지금부터 예시 코드를 보여드리겠습니다.

```Gleam
import File

fn read_file() {
  let file = File.open("file.txt")
  let contents = File.read_all(file)
  contents
}
```

위 코드는 "file.txt"라는 파일을 읽어서 `contents` 변수에 저장하고 반환하는 함수입니다. 코드를 실행해보면 `contents`에 파일 내용이 저장되어 출력됩니다.

```
Hello World!
This is a sample text file.
```

## 깊이 파기

파일을 읽는 방법은 여러 가지가 있습니다. 예를 들어, `File.read_line`을 사용하면 파일의 한 줄씩 읽어올 수 있습니다. 또한 `File.read_until`을 사용하면 특정 문자나 단어가 등장할 때까지 파일을 읽을 수 있습니다. 더 자세한 정보는 Gleam 문서를 참고해주세요.

## 참고

만약 파일을 읽는 기술에 대해 궁금하시다면 아래 링크들을 참고해주세요.

- [Gleam 문서 - File 모듈](https://gleam.run/stdlib/file.html)
- [Gleam 문서 - 텍스트 파일 다루기](https://gleam.run/book/tutorials/file-io.html#reading-text-files)
- [Gleam 문서 - 오픈 표준 라이브러리](https://gleam.run/stdlib/)