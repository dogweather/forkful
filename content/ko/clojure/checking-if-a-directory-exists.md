---
title:    "Clojure: 디렉토리가 존재하는지 확인하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 왜

폴더가 존재하는지 확인하는 것이 왜 필요한지 궁금하십니까? 이 글에서는 그 이유를 소개하고 코딩 예제를 통해 어떻게 확인할 수 있는지 자세히 알아보겠습니다.

## 왜 폴더가 존재하는지 확인해야 하는가?

폴더가 존재하는지 확인하는 것은 파일 시스템과 관련된 작업에서 매우 중요합니다. 예를 들어, 파일을 읽거나 쓰기 전에 폴더가 존재하는지 확인하고, 없는 경우에는 생성하거나 다른 작업을 수행할 수도 있습니다. 또는 특정 폴더에 이미 파일이 있는지 확인하고, 존재하지 않는 경우 다른 폴더를 사용하여 작업하는 등의 상황에서 사용할 수 있습니다.

## 어떻게 확인할 수 있나요?

```clojure
(ns example.core
  (:require [clojure.java.io :as io]))

(defn folder-exists? [folder]
  (.exists (io/file folder)))

; 폴더가 존재하는 경우
(folder-exists? "/Users/Username/Documents")
; => true

; 폴더가 존재하지 않는 경우
(folder-exists? "/Users/Username/Desktop")
; => false
```

위 예제에서는 `clojure.java.io` 네임스페이스를 사용하여 폴더가 존재하는지를 확인하고 있습니다. 먼저 `require` 문을 통해 네임스페이스를 가져오고, `defn`을 사용하여 `folder-exists?`라는 함수를 정의하고 있습니다. 이 함수는 `io/file`을 통해 해당 폴더의 파일 객체를 생성한 후 `.exists` 메서드를 이용하여 폴더가 존재하는지를 확인하고 있습니다.

## 깊게 들어가기

파일을 다루는 작업을 할 때 폴더가 존재하는지 확인하는 것은 중요하지만, 이보다 더 중요한 것은 다루는 파일의 권한을 확인하는 것입니다. `(.exists ...)` 메서드는 폴더 뿐만 아니라 파일의 존재 여부도 확인할 수 있고, `(.canRead ...)`과 `(.canWrite ...)` 메서드를 통해 해당 파일의 읽기와 쓰기 권한 여부를 확인할 수 있습니다. 이를 조합하여 폴더가 존재하지만 읽고 쓰기가 불가능한 경우에도 처리할 수 있습니다.

# 참고

- [Clojure Documentation: IO](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Clojure by Example: File I/O](https://kimh.github.io/clojure-by-example/#file-io)