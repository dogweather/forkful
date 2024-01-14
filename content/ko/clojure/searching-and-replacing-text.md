---
title:    "Clojure: 텍스트 검색 및 교체"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 대체하는 것의 중요성은 프로그래밍에서 가장 기본적인 작업 중 하나입니다. 이를 통해 텍스트를 자동으로 수정하고 유지 관리할 수 있으며, 시간과 노력을 절약할 수 있습니다.

## 방법

### 텍스트 검색하기

Clojure에서는 `re-seq` 함수를 사용하여 정규표현식을 사용해 텍스트를 검색할 수 있습니다.

```Clojure
(re-seq #"[0-9]+" "Hello123")
;;=> ("123")
```

### 텍스트 대체하기

`replace` 함수를 사용하여 텍스트를 대체할 수 있습니다.

```Clojure
(def text "Clojure is awesome!")
(replace text #"awesome" "amazing")
;;=> "Clojure is amazing!"
```

## Deep Dive

Clojure에서는 정규표현식을 사용하여 복잡한 패턴을 찾고 대체하는 것이 가능합니다. 또한 `replace-first` 함수를 사용하여 첫 번째로 매칭되는 패턴만 대체할 수도 있습니다. 또한 `re-pattern` 함수를 사용하여 정규표현식을 빠르게 컴파일할 수 있습니다.

## See Also

- [re-seq 함수 문서](https://clojuredocs.org/clojure.core/re-seq)
- [replace 함수 문서](https://clojuredocs.org/clojure.core/replace)
- [정규표현식 레퍼런스](https://www.regular-expressions.info/)