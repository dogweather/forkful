---
title:                "Clojure: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜 디렉토리가 존재하는지 확인할까요?

파일 또는 폴더를 다루다보면 가끔씩 그것이 존재하는지 여부를 확인해야하는 경우가 있습니다. 디렉토리가 이미 존재하는지 확인하는 것은 응용프로그램이나 스크립트에서 더 복잡한 로직을 구현할 때 매우 유용합니다.

## 어떻게 확인할까요?

Clojure에는 [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html) 라이브러리를 통해 디렉토리를 확인할 수 있는 함수가 있습니다. 아래는 예시 코드와 함께 실행 결과입니다.

```Clojure
(ns checking-directory-exists.blog
  (:require [clojure.java.io :as io]))

;; 디렉토리가 존재하는지 여부를 확인하는 함수를 정의합니다.
(defn is-dir? [path]
  (.isDirectory (java.io.File. path)))

;; 존재하는 디렉토리를 매개변수로 함수를 호출합니다.
(is-dir? "/Users/username/Documents")
;; 출력 결과: true

;; 존재하지 않는 디렉토리를 매개변수로 함수를 호출합니다.
(is-dir? "/Users/username/Downloads")
;; 출력 결과: false
```

위 예제에서는 `is-dir?` 함수를 통해 파일이나 폴더의 경로를 전달하고, 해당 경로의 디렉토리가 존재하는지 여부를 확인할 수 있습니다.

## 딥 다이브

디렉토리가 존재하는지 확인하는 함수의 내부 구현을 살펴보겠습니다. Clojure의 [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html) 라이브러리에서는 `clojure.java.io/file` 함수를 통해 파일 또는 디렉토리의 `File` 객체를 생성합니다. `java.io.File` 클래스에는 `.isDirectory` 메소드가 정의되어 있으며, 이 메소드는 파일이나 디렉토리가 존재하면 `true`를, 그렇지 않으면 `false`를 반환합니다. 따라서 `is-dir?` 함수는 Clojure의 `clojure.java.io/file` 함수와 Java의 `java.io.File` 클래스에서 지원하는 메소드를 조합하여 구현한 것입니다.

# 참고 자료

- [Official Clojure docs on checking directory existence](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- [Java documentation for java.io.File class](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)