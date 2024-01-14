---
title:    "Clojure: 텍스트 파일 읽기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 읽는 것은 프로그래밍에서 중요한 스킬입니다. 파일 시스템과 통신하는 방법을 배우는 것은 중요한 능력이며, 데이터를 처리하는 데에도 매우 유용합니다.

## 어떻게?

텍스트 파일을 읽는 Clojure의 기본 기능은 `slurp` 함수를 사용하는 것입니다. 이 함수는 지정된 파일의 내용을 하나의 문자열로 읽어옵니다. 예제 코드는 다음과 같습니다:

```Clojure
(def file-content (slurp "sample.txt"))
```

만약 파일의 내용이 매우 크다면, 메모리 오버헤드가 발생할 수 있습니다. 이럴 때에는 `with-open` 함수를 사용하여 파일을 한 줄씩 읽을 수 있습니다. 예제 코드는 다음과 같습니다:

```Clojure
(with-open [reader (reader "sample.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

위의 코드에서 `line-seq` 함수는 파일을 한 줄씩 읽어오는 함수입니다. `doseq`는 각 줄을 반복하면서 `println` 함수를 사용하여 화면에 출력합니다.

## 딥 다이브

파일을 읽는 데에는 여러 가지 방법이 있을 수 있습니다. 예를 들어, `clojure.java.io` 네임스페이스에는 파일을 읽고 쓰는 데에 유용한 함수들이 많이 포함되어 있습니다. 그리고 `clojure.core` 네임스페이스에도 여러 가지 파일 처리 함수들이 있습니다. 이러한 다양한 함수들을 사용하면 더 다양한 방식으로 파일을 읽을 수 있습니다.

## 또 다른 정보

- [ClojureDocs - slurp](https://clojuredocs.org/clojure.core/slurp)
- [ClojureDocs - with-open](https://clojuredocs.org/clojure.core/with-open)
- [ClojureDocs - line-seq](https://clojuredocs.org/clojure.core/line-seq)
- [ClojureDocs - io/reader](https://clojuredocs.org/clojure.java.io/io#v5983)
- [ClojureDocs - io/writer](https://clojuredocs.org/clojure.java.io/writer)