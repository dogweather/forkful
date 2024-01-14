---
title:                "Clojure: 디렉토리가 존재하는지 확인하기"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 중요한 이유는 코드의 안정성을 보장하기 위해서 입니다. 프로그램에서 디렉토리가 존재하는지 확인하면, 사용자들이 원하는 파일이나 데이터를 정확하게 접근할 수 있게 됩니다.

## 방법

아래의 예제 코드를 이용하여 디렉토리가 존재하는지 확인하는 방법을 살펴보겠습니다.

```Clojure
(ns checking-directory-test.core
  (:require [clojure.java.io :as io]))

;; 디렉토리가 존재하는지 확인하는 함수
(defn check-directory [path]
  (when (.exists (io/file path))
    (println "해당 디렉토리가 존재합니다.")
    ;; 디렉토리의 절대 경로를 반환
    (.getAbsolutePath (io/file path))))

;; 예제: 디렉토리 존재 여부를 확인
(check-directory "resources")
;; => "해당 디렉토리가 존재합니다.
       /Users/example/project/resources"

(check-directory "nonexistent/directory")
;; => nil (존재하지 않는 디렉토리는 nil을 반환합니다.)
```

## 깊게 빠져드는

위의 코드 예제에서, `exists` 함수를 이용하여 디렉토리가 존재하는지 확인하고, 절대 경로를 반환합니다. 만약 존재하지 않는 디렉토리를 확인하면 nil을 반환합니다. 이를 이용하여 프로그램에서 원하는 작업을 수행할 수 있습니다. 예를 들어, 만약 디렉토리가 존재하지 않는다면, 사용자에게 알리는 메시지를 표시하거나 새로운 디렉토리를 만들 수도 있습니다.

## 참고

- [Clojure Cookbook](https://clojure-cookbook.com/)의 [Working with Directories](https://clojure-cookbook.com/dirs/) 섹션
- [ClojureDocs](https://clojuredocs.org/)의 [clojure.java.io 라이브러리 문서](https://clojuredocs.org/clojure.java.io)