---
title:                "컴퓨터 프로그래밍의 제목은 텍스트 파일 작성입니다."
html_title:           "Clojure: 컴퓨터 프로그래밍의 제목은 텍스트 파일 작성입니다."
simple_title:         "컴퓨터 프로그래밍의 제목은 텍스트 파일 작성입니다."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 지금 무엇을 & 왜?
텍스트 파일 작성은 단순히 컴퓨터에 글을 쓰는 것입니다. 프로그래머들은 자신의 코드나 데이터를 보관하고 공유하기 위해서 텍스트 파일을 작성합니다.

# 어떻게:
```Clojure
(with-open [file (io/writer "/path/to/file.txt")]
  (.write file "Hello World!")
  (.close file))
```

파일을 열고 쓰기 위해서 `with-open` 함수를 사용합니다. 이 함수는 파일의 경로를 첫 번째 인자로 받아서 파일을 열고 두 번째 인자로 받은 함수를 실행합니다. 여기서는 파일을 쓰는 함수를 실행합니다. 해당 함수에 파일 객체와 쓸 내용을 전달합니다. 마지막으로 파일을 닫습니다.

# 깊이 파보기:
텍스트 파일 작성은 컴퓨터 프로그래밍에서 매우 중요한 기술입니다. 예전에는 파일을 쓰는 것이 다소 복잡했지만 요즘에는 매우 쉽고 간단해졌습니다. `with-open` 함수는 파일을 열고 닫는 번거로운 작업을 알아서 처리해 줍니다. 이런 방식으로 코드를 작성하면 실수를 줄이고 코드를 더 간결하게 만들 수 있습니다.

# 관련 자료:
- [Clojure 공식 문서 - 파일 다루기](https://clojure.org/reference/io)
- [Clojure Cookbook - 파일 다루기](https://clojure-cookbook.com/06-io/06-07)
- [Clojure Style Guide - 파일 다루기](https://github.com/bbatsov/clojure-style-guide#files-and-folders)