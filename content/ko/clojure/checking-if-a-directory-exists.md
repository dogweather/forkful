---
title:    "Clojure: 디렉토리 존재 여부 확인"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 중요한지 궁금하지 않으신가요? 프로그래밍을 할 때 디렉토리가 존재하는지 확인하는 것은 코드를 안전하고 효율적으로 작성할 수 있도록 도와줍니다.

## 방법

아래 예제 코드를 살펴보세요!

```Clojure
(require '[clojure.java.io :as io])

;; 디렉토리가 존재하는지 확인합니다.
(.isDirectory (io/file "sample-directory")) ;=> true

;; 디렉토리가 존재하지 않는 경우 false를 반환합니다.
(.isDirectory (io/file "non-existing-directory")) ;=> false
```

위 코드에서는 `clojure.java.io` 라이브러리의 `file`과 `isDirectory` 함수를 사용하여 디렉토리를 체크하고, 존재 여부에 따라 `true` 또는 `false`를 반환합니다.

## 깊이 드라이브

하지만 디렉토리의 존재 여부를 확인하는 것만으로는 부족합니다. 때로는 디렉토리를 생성하거나 삭제하는 작업도 필요할 수 있습니다. 이를 위해 `clojure.java.io` 라이브러리에는 `make-directory`와 `delete-directory` 함수도 제공합니다.

```Clojure
;; 새로운 디렉토리를 생성합니다.
(mkdir "new-directory") ;=> nil

;; 디렉토리를 삭제합니다.
(delete-directory "new-directory") ;=> true
```

또한, `java.nio.file` 라이브러리를 사용하여 디렉토리를 생성하거나 삭제할 수도 있습니다. 자세한 내용은 [여기](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)에서 확인할 수 있습니다.

## 함께 보기

- [Clojure 공식 문서](https://clojure.org/)
- [Clojure 공식 가이드](https://clojure.org/guides/learn/syntax)
- [clojure.java.io 라이브러리 문서](https://clojure.github.io/java.io/)
- [java.nio.file 라이브러리 문서](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)