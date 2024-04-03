---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:44.748533-07:00
description: "Clojure\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\
  \uD558\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC5B4\uD50C\uB9AC\uCF00\uC774\
  \uC158 \uC678\uBD80\uC5D0 \uC800\uC7A5\uD558\uC5EC \uC9C0\uC18D\uC131, \uC124\uC815\
  , \uB85C\uAE45 \uB610\uB294 \uD504\uB85C\uC138\uC2A4 \uAC04 \uD1B5\uC2E0\uC744 \uAC00\
  \uB2A5\uD558\uAC8C \uD558\uB294 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uAC70\uB098\
  \ \uC218\uC815\uD558\uB294 \uC791\uC5C5\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158 \uC0C1\uD0DC\
  , \uC124\uC815\uC744 \uC678\uBD80\uD654\uD558\uAC70\uB098 \uD504\uB85C\uADF8\uB7A8\
  \uC758 \uB2E4\uB978 \uBD80\uBD84 \uB610\uB294 \uC804\uD600\u2026"
lastmod: '2024-03-13T22:44:54.688111-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\
  \uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC5B4\uD50C\uB9AC\uCF00\uC774\uC158\
  \ \uC678\uBD80\uC5D0 \uC800\uC7A5\uD558\uC5EC \uC9C0\uC18D\uC131, \uC124\uC815,\
  \ \uB85C\uAE45 \uB610\uB294 \uD504\uB85C\uC138\uC2A4 \uAC04 \uD1B5\uC2E0\uC744 \uAC00\
  \uB2A5\uD558\uAC8C \uD558\uB294 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uAC70\uB098\
  \ \uC218\uC815\uD558\uB294 \uC791\uC5C5\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:


### Clojure의 내장 함수를 사용하여 파일에 텍스트 쓰기
`spit` 함수는 Clojure에서 파일에 텍스트를 쓰는 가장 간단한 방법입니다. 이 함수는 두 가지 인수를 받습니다: 파일 경로와 쓸 문자열. 파일이 없으면 `spit`은 그것을 생성합니다. 이미 있다면, `spit`은 그것을 덮어씁니다.

```clojure
(spit "example.txt" "Hello, world!")
```

기존 파일에 텍스트를 추가하려면, `spit` 함수에 `:append` 옵션을 사용할 수 있습니다.

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

이 스니펫을 실행한 후, "example.txt"는 다음과 같은 내용을 포함하게 됩니다:

```
Hello, world!
Let's add this new line.
```

### 서드 파티 라이브러리 사용하기
Clojure의 내장 기능이 종종 충분하지만, 커뮤니티는 더 복잡하거나 특정한 작업을 위한 강력한 라이브러리를 개발했습니다. 파일 I/O를 위한 인기 있는 라이브러리 중 하나는 `clojure.java.io`로, 파일 처리를 좀 더 자바 스타일로 접근할 수 있게 해줍니다.

`clojure.java.io`를 사용하여 파일에 쓰려면, 먼저 이를 가져와야 합니다:

```clojure
(require '[clojure.java.io :as io])
```

그런 다음, `writer` 함수를 사용하여 writer 객체를 얻고, `spit` 함수(또는 `print`, `println` 같은 다른 함수들)를 사용하여 파일에 쓸 수 있습니다:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "This is written using clojure.java.io"))
```

이렇게 하면 "example_with_io.txt" 파일이 생성되며 (이미 존재한다면 덮어쓰기 됨), 다음의 텍스트를 포함하게 됩니다:

```
This is written using clojure.java.io
```

기억하세요: `with-open`은 쓰기 작업 후 파일이 제대로 닫혀서 잠재적인 리소스 누수를 방지합니다.
