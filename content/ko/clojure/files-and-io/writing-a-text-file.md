---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:44.748533-07:00
description: "\uBC29\uBC95: `spit` \uD568\uC218\uB294 Clojure\uC5D0\uC11C \uD30C\uC77C\
  \uC5D0 \uD14D\uC2A4\uD2B8\uB97C \uC4F0\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\
  \uBC95\uC785\uB2C8\uB2E4. \uC774 \uD568\uC218\uB294 \uB450 \uAC00\uC9C0 \uC778\uC218\
  \uB97C \uBC1B\uC2B5\uB2C8\uB2E4: \uD30C\uC77C \uACBD\uB85C\uC640 \uC4F8 \uBB38\uC790\
  \uC5F4. \uD30C\uC77C\uC774 \uC5C6\uC73C\uBA74 `spit`\uC740 \uADF8\uAC83\uC744 \uC0DD\
  \uC131\uD569\uB2C8\uB2E4. \uC774\uBBF8 \uC788\uB2E4\uBA74, `spit`\uC740 \uADF8\uAC83\
  \uC744 \uB36E\uC5B4\uC501\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.688111-06:00'
model: gpt-4-0125-preview
summary: "`spit` \uD568\uC218\uB294 Clojure\uC5D0\uC11C \uD30C\uC77C\uC5D0 \uD14D\uC2A4\
  \uD2B8\uB97C \uC4F0\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC785\uB2C8\
  \uB2E4."
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
