---
date: 2024-01-20 17:40:07.307774-07:00
description: "How to: (\uBC29\uBC95) Clojure\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\
  \uC744 \uB9CC\uB4E4\uB824\uBA74 `java.io.File`\uC758 `createTempFile` \uBA54\uC11C\
  \uB4DC\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC608\uC81C\uB97C \uD655\uC778\uD574\
  \ \uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.526994-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Clojure\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\
  \uB4E4\uB824\uBA74 `java.io.File`\uC758 `createTempFile` \uBA54\uC11C\uB4DC\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## How to: (방법)
Clojure에서 임시 파일을 만들려면 `java.io.File`의 `createTempFile` 메서드를 사용합니다. 예제를 확인해 보세요.

```clojure
(require '[clojure.java.io :as io])

; 임시 파일 생성
(def temp-file (io/file (java.io.File/createTempFile "temp" ".txt")))

; 사용 예 - 단어 목록을 임시 파일에 씁니다.
(spit temp-file "clojure\njava\nlisp\n")

; 임시 파일 읽기
(slurp temp-file)
; => "clojure\njava\nlisp\n"
```

만들어진 임시 파일은 프로그램이 종료될 때 시스템에 의해 자동으로 삭제됩니다.

## Deep Dive (심층 분석)
임시 파일 생성은 UNIX 시스템에서 시작되어 널리 사용되는 기능입니다. `java.io.File/createTempFile` 메서드는 자바에서 제공하며, Clojure는 자바 플랫폼 위에서 실행되므로 이 메서드를 이용할 수 있습니다. 임시 파일은 일반적으로 시스템의 특정 폴더에 생성되는데, 직접 위치를 지정할 수도 있습니다. 대체 방법으로는 NIO 패키지의 `Files/createTempFile`을 사용할 수도 있습니다. 하지만 임시 파일 사용 시 보안을 중요시해야 하며 악의적인 프로그램에 의해 사용될 위험도 있습니다.

## See Also (더 보기)
- [Java Platform SE 8 java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [ClojureDocs `spit`](https://clojuredocs.org/clojure.core/spit)
- [ClojureDocs `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [Oracle's Guide to java.nio.file.Files](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
