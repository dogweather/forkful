---
title:                "Clojure: 임시 파일 만들기"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
임시 파일을 생성하는 일에 관심을 가질 이유는 무엇일까요? 그 이유는 임시 파일을 사용하여 프로그램의 실행을 더 효율적으로 조작할 수 있기 때문입니다. 임시 파일은 프로그램이 실행되는 동안 일시적으로 사용되며, 사용이 끝나면 자동으로 삭제됩니다.

## 어떻게
임시 파일을 생성하는 방법은 간단합니다. 우선, `clojure.java.io` 네임스페이스를 불러옵니다. 그 다음, `temp-file` 함수를 사용하여 임시 파일의 경로를 지정합니다. 예제 코드와 출력은 아래와 같습니다.

```Clojure
(ns temp-file.example
  (:require [clojure.java.io :as io]))

(def temp-file (io/temp-file "example"))

(print temp-file)
```

```
Example output: /var/folders/1r/6mckl0xn3wzchpzxv_kl168h0000gn/T/clojure7391417424787439545.clj
```

위의 예제 코드에서 `temp-file` 함수의 인자로 사용된 "example"은 파일명으로 사용됩니다. 따라서 본인의 파일명으로 변경하셔도 괜찮습니다. 또한, 파일을 생성할 디렉토리를 지정하고 싶다면 `:dir` 키워드를 사용하면 됩니다. 자세한 내용은 아래의 Deep Dive 섹션에서 확인할 수 있습니다.

## Deep Dive
`temp-file` 함수는 실제로 `make-temp-file` 함수를 호출하여 임시 파일을 생성합니다. 이 함수는 3개의 인자를 가지고 있으며, 첫 번째 인자는 파일명, 두 번째 인자는 생성할 임시 파일의 디렉토리 경로, 세 번째 인자는 파일의 확장자를 지정하는 문자열입니다. 만약, 세 번째 인자가 생략되면 ".tmp" 확장자가 기본적으로 사용됩니다. `temp-file`함수를 사용하는 것과 마찬가지로, `make-temp-file` 함수를 사용하여 다양한 옵션을 설정할 수 있습니다.

## 더 알아보기
여러분이 클로저로 임시 파일을 생성하는 방법에 대해서 더 자세히 알고싶다면 아래의 자료를 참고하시기 바랍니다.

- [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [ClojureDocs - make-temp-file](https://clojuredocs.org/clojure.java.io/make-temp-file)
- [ClojureDocs - temp-file](https://clojuredocs.org/clojure.java.io/temp-file)

## 참고 자료
- [ClojureDocs - temp-file](https://clojuredocs.org/clojure.java.io/temp-file)
- [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)