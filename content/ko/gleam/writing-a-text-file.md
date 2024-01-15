---
title:                "텍스트 파일 작성하기"
html_title:           "Gleam: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 쓰는 것의 이점은 데이터를 잘 구분하기 위한 복잡한 파일 형식보다는 단순한 텍스트 형식으로 정보를 저장할 수 있기 때문입니다.

## 하우 투

```Gleam
/*
 모듈을 포함하고 파일을 연다.
 어떤 파일이든 열 수 있습니다.
*/
use gleam/file

file.open("test.txt")

/*
 write_line 함수를 사용하여 파일에 텍스트를 추가합니다.
*/
file.write_line("|- 안녕하세요")
file.append_line("|- Gleam으로 작성한 파일입니다.")
file.close()

/*
 파일을 읽어서 콘솔에 출력합니다.
*/
let text = file.read_line("test.txt")
IO.println(text)
```

```
안녕하세요
Gleam으로 작성한 파일입니다.
```

## 딥 다이브

텍스트 파일을 작성하고 읽는 것은 Gleam에서 매우 간단합니다. 파일 모듈은 다양한 파일 작업을 지원하고있어 파일 생성, 수정, 삭제 등 다양한 작업을 할 수 있습니다. 또한 파일 내용을 이용하여 데이터 처리를 할 수도 있습니다.

# 참고 자료

* [Gleam 공식 문서](https://github.com/gleam-lang/gleam/tree/master/docs)
* [Gleam 예제 코드](https://github.com/gleam-lang/gleam/tree/master/examples)