---
title:                "Gleam: 임시 파일 생성"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Gleam 언어로 임시 파일을 만드는 방법

## 왜

임시 파일을 만드는 것의 가장 큰 이유는 프로그램이 파일에 데이터를 읽고 쓸 수 있도록 하는 것입니다. 이는 프로그램이 실행 중에 만들어진 파일을 활용할 수 있게 해줍니다.

## 어떻게

임시 파일을 만드는 방법은 간단합니다. 먼저, 해당 파일이 저장될 디렉토리를 정하고, 임의의 이름을 지정해주면 됩니다. 그리고 다음과 같이 코드를 작성하면 됩니다: 

```Gleam
import gleam/os

// 임시 파일을 만듭니다
let temp_file = os.temp_file("/tmp/", "gleam_temp_")

// 임시 파일에 데이터를 쓰고 읽습니다
os.write_file(temp_file, "안녕하세요")
let data = os.read_file(temp_file)

// 결과를 출력합니다
io.println(data)
```

위 코드를 실행하면 다음과 같은 결과가 출력됩니다:

```
안녕하세요
```

## Deep Dive

임시 파일을 만드는 더 깊은 내용을 알고 싶다면, Gleam 공식 문서에서 관련 정보를 찾아볼 수 있습니다. 이 문서에서는 임시 파일을 만드는 다른 방법도 소개해줍니다. 예를 들어, 운영체제에 기본적으로 포함되어 있는 `mktemp` 명령어를 사용하여 임시 파일을 생성하는 방법도 있습니다.

## 비슷한 주제

- [Gleam 공식 문서: 임시 파일 만들기](https://gleam.run/manual/stdlib.html#io.temp-file)
- [Linux 명령어: mktemp](https://man7.org/linux/man-pages/man1/mktemp.1.html)

---

# 더 알아보기

- [Gleam 공식 웹사이트](https://gleam.run/)
- [Gleam 공식 GitHub 레포지토리](https://github.com/gleam-lang/gleam)