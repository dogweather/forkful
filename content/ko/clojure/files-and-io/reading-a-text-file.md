---
date: 2024-01-20 17:53:54.165278-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD560\uAE4C\uC694?) Clojure\uC5D0\uC11C\
  \ \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC27D\uAC8C \uC77D\uC744 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. `slurp` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uBA74 \uC804\uCCB4 \uB0B4\
  \uC6A9\uC744 \uBB38\uC790\uC5F4\uB85C \uBD88\uB7EC\uC62C \uC218 \uC788\uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.524586-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD560\uAE4C\uC694?) Clojure\uC5D0\uC11C \uD14D\uC2A4\
  \uD2B8 \uD30C\uC77C\uC744 \uC27D\uAC8C \uC77D\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (어떻게 할까요?)
Clojure에서 텍스트 파일을 쉽게 읽을 수 있습니다. `slurp` 함수를 사용하면 전체 내용을 문자열로 불러올 수 있죠.

```Clojure
;; 전체 파일을 한 번에 읽기
(slurp "example.txt")
```

파일이 큰 경우, `line-seq`와 함께 `reader`를 사용하여 라인별로 읽을 수 있습니다.

```Clojure
;; 파일을 라인별로 읽기
(with-open [rdr (reader "example.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```

샘플 출력:

```
"첫 번째 줄"
"두 번째 줄"
"세 번째 줄"
```

## Deep Dive (심층 분석)
`slurp` 함수는 Clojure의 가장 간단한 파일 읽기 방법입니다. 하지만 큰 파일을 처리할 때는 메모리 문제를 일으킬 수 있습니다. `line-seq`와 `reader`는 파일을 순차적으로 읽어 이 문제를 해결합니다.

역사적으로 파일 읽기는 대부분의 프로그래밍 언어에서 필수적인 기능이었습니다. `slurp`와 `line-seq` 같은 함수는 이를 추상화하여 Clojure 프로그래머들에게 간편한 인터페이스를 제공합니다.

읽기 옵션을 더 다양하게 제어하려면 Java의 I/O 라이브러리를 직접 사용할 수도 있습니다. 그러나 Clojure 함수들은 대부분의 경우 충분합니다.

## See Also (참고자료)
- Clojure 공식 문서: [Clojure Docs](https://clojure.org/api/api)
- Java I/O에 대한 더 깊은 이해: [Java I/O Tutorial](https://docs.oracle.com/javase/tutorial/essential/io/)
- Clojure의 파일 시스템 라이브러리인 `clojure.java.io`: [clojure.java.io API](https://clojure.github.io/clojure/clojure.java.io-api.html)
