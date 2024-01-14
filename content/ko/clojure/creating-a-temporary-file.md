---
title:                "Clojure: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 임시 파일을 만들까요?

임시 파일은 프로그래밍에서 자주 사용되는 중요한 개념입니다. 임시 파일은 일시적으로 데이터를 저장하고 관리하는 데 사용됩니다. 이러한 임시 파일은 일반적으로 프로그램 실행 중에 생성되고 사용이 필요하지 않으면 삭제됩니다.

# 만드는 방법

임시 파일을 만드는 가장 간단한 방법은 `clojure.java.io` 패키지에서 제공하는 `temp-file` 함수를 사용하는 것입니다. 이 함수는 2개의 인자를 받아들입니다. 첫 번째는 임시 파일의 접두사이고 두 번째는 접미사입니다. 아래는 해당 함수를 사용한 예시 코드입니다.

```Clojure
(require '[clojure.java.io :as io])

(def tmp-file (io/temp-file "prefix-" "-suffix"))
```

위 코드를 실행하면 `"prefix-randomsuffix"` 형태의 임시 파일이 생성됩니다. 또한 해당 파일은 기본적으로 시스템의 임시 디렉토리에 저장됩니다. 만약 파일을 다른 디렉토리에 저장하고 싶다면 `temp-file` 함수의 3번째 인자로 해당 디렉토리의 경로를 전달하면 됩니다.

# 깊게 들어가기

임시 파일을 생성하는 것만으로는 충분하지 않을 수 있습니다. 때문에 더 많은 옵션을 제공하는 `with-temporary-file` 함수를 사용할 수도 있습니다. 이 함수는 `temp-file` 함수와 같은 인자를 받아들이지만, 임시 파일을 사용하는 범위를 제한할 수 있도록 도와줍니다. 예를 들어, 아래 코드는 임시 파일을 생성하고 해당 파일에서 데이터를 읽은 뒤에 파일을 자동으로 삭제합니다.

```Clojure
(require '[clojure.java.io :as io])

(io/with-temporary-file "prefix-" "-suffix"
  (fn [tmp-file]
    (with-open [file (io/reader tmp-file)]
      (println (slurp file))
      )))
```

# 또 다른 정보

- [Clojure 공식 문서 - clojure.java.io](https://clojure.org/reference/java_interop#_file_io)
- [장시간 사용되지 않는 객체 제거시스템(Display::Win32DisplaySystem)문서](https://en.wikibooks.org/wiki/Clojure_Programming/Destructuring)See Also: 드스트링처링(destructuring)에 대한 더 깊은 정보를 원한다면 다음 링크를 확인해보세요.