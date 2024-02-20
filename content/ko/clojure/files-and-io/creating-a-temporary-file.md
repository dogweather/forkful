---
date: 2024-01-20 17:40:07.307774-07:00
description: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uC740 \uB370\uC774\uD130\uB97C\
  \ \uC77C\uC2DC\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD55C \uD30C\uC77C\
  \uC744 \uB9CC\uB4DC\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB294 \uC8FC\uB85C \uB370\uC774\uD130 \uCC98\uB9AC \uC911\uAC04 \uACB0\uACFC\uB97C\
  \ \uBCF4\uAD00\uD558\uAC70\uB098, \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uAC04 \uB370\
  \uC774\uD130 \uC804\uB2EC \uC2DC \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.631872
model: gpt-4-1106-preview
summary: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uC740 \uB370\uC774\uD130\uB97C \uC77C\
  \uC2DC\uC801\uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD55C \uD30C\uC77C\uC744\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uC8FC\uB85C \uB370\uC774\uD130 \uCC98\uB9AC \uC911\uAC04 \uACB0\uACFC\uB97C \uBCF4\
  \uAD00\uD558\uAC70\uB098, \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uAC04 \uB370\uC774\
  \uD130 \uC804\uB2EC \uC2DC \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
임시 파일 생성은 데이터를 일시적으로 저장하기 위한 파일을 만드는 것입니다. 프로그래머는 주로 데이터 처리 중간 결과를 보관하거나, 애플리케이션간 데이터 전달 시 사용합니다.

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
