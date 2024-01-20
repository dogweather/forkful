---
title:                "디렉토리의 존재 여부 확인하기"
html_title:           "Arduino: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉토리 존재 여부를 확인하는 것은, 파일 시스템에서 특정 경로가 실제로 존재하는 폴더인지를 검사하는 과정입니다. 이를 통해 프로그래머는 파일 조작이나 디렉토리 관리를 올바르게 할 수 있어서, 예기치 못한 에러를 방지할 수 있습니다.

## How to: (방법)
Clojure에서는 `java.io.File` 클래스를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다. 여기 간단한 예제가 있습니다:

```clojure
(import '(java.io File))

(defn directory-exists? [path]
  (let [dir (File. path)]
    (.exists dir)))

;; 예제 사용
(println (directory-exists? "/path/to/dir")) ; 존재하면 true, 아니면 false
```
코드를 실행하면, 존재하는 디렉토리에 대해 `true`가, 존재하지 않는 경우 `false`가 출력됩니다.

## Deep Dive (심도 있는 분석)
Clojure는 JVM 위에서 동작하기 때문에 자바의 표준 라이브러리를 활용한 위와 같은 방법이 일반적입니다. 과거에는 `clojure.java.io/file`, `clojure.java.io/as-file` 같은 기능을 사용했지만, 현재는 바로 `java.io.File`을 쓰는 것이 널리 퍼진 방식입니다. 또 다른 방법으로 `nio.file.Files/exist`를 사용할 수도 있지만, 일상적인 Clojure 작업에서는 약간 과한 편입니다. 

가장 중요한 점은, 프로그램이 디렉토리의 존재 여부만을 확인한다면 큰 리소스를 사용하지 않는다는 것입니다. 애플리케이션의 안정성을 확보하면서 최소한의 리소스를 사용하는 것은 프로그래머가 고려해야 할 중요한 사항 중 하나입니다.

## See Also (참고 자료)
- Clojure 공식 문서: [https://clojure.org/](https://clojure.org/)
- 자바 `File` API 문서: [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Stack Overflow Clojure 태그: [https://stackoverflow.com/questions/tagged/clojure](https://stackoverflow.com/questions/tagged/clojure)