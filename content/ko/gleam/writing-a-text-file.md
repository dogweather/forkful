---
title:    "Gleam: 텍스트 파일 작성하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 확실히 코드나 데이터를 구조화하고 저장하기 위해서입니다.

## 어떻게 작성하는지

```Gleam
import gleam/io

fn main() {
  let text = "안녕하세요! 이것은 Gleam 블로그 글입니다."
  let file = "blog.txt"
  io.write_file(text, file)
}
```

위 코드를 사용하면 "blog.txt"라는 이름의 파일을 만들고 "안녕하세요! 이것은 Gleam 블로그 글입니다."라는 내용을 넣을 수 있습니다.

## 깊이 파헤치기

텍스트 파일을 작성할 때는 파일을 열고 내용을 작성한 뒤, 파일을 닫는 일련의 과정이 필요합니다. 이 과정을 "I/O"라고 부릅니다. Gleam 라이브러리는 이러한 과정을 단순화하고 쉽게 사용할 수 있도록 도와줍니다.

## 바로가기

[파일 입출력에 대한 자세한 설명](https://gleam.run/documentation/stdlib/io)을 확인해보세요.

## 같이 보기
- [Gleam 공식 웹사이트](https://gleam.run/)
- [Gleam 기초 프로그래밍 가이드](https://gleam.run/tutorial/getting-started)
- [블로그 글 작성을 위한 Markdown 사용법](https://www.markdownguide.org/basic-syntax/)