---
title:                "디버그 출력하는 것"
html_title:           "Clojure: 디버그 출력하는 것"
simple_title:         "디버그 출력하는 것"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 뭐고 왜?
디버그 출력(printing debug output)이란 무엇인지 알아보고 프로그래머들이 왜 이것을 사용하는지 알아봅시다.

## 어떻게:
디버그 출력을 사용하는 예시와 샘플 출력을 ```Clojure ... ``` 코드 블록 내에서 살펴봅시다.

```Clojure
;; 예시 코드
(defn add [a b]
  (println "Adding" a "and" b)
  (+ a b))

(def result (add 5 7))
;; 출력: Adding 5 and 7
;; 결과: 12
```

## 깊이 들어가기:
디버그 출력을 사용하는 바탕과 대안, 그리고 디버그 출력의 구현 세부사항을 알아봅시다.

### 바탕:
디버그 출력은 코드의 실행 과정에서 발생하는 오류를 찾고 수정하는 데 도움이 됩니다. 디버그 출력은 코드에서 어떤 부분이 실행되고 있는지, 변수 값이 어떻게 변하는지 등을 알려주어 오류를 찾는 데 유용합니다.

### 대안:
디버그 출력 외에도 디버깅을 위해 디버거를 사용할 수 있습니다. 디버거는 코드를 한 줄씩 실행하면서 오류를 찾아주는 도구입니다. 그러나 디버깅의 경우에는 디버거를 사용하는 것이 복잡하기 때문에, 간단한 코드에서는 디버그 출력이 더 쉽고 효과적일 수 있습니다.

### 구현 세부사항:
Clojure에서 디버그 출력을 하기 위해서는 ```println``` 함수를 사용하면 됩니다. 그러나 디버그 출력은 코드의 실행 속도를 느리게 할 수 있으므로, 디버그 작업이 끝난 후에는 삭제하거나 주석 처리하는 것이 좋습니다.

## 관련 자료:
- [Clojure 공식 문서](https://clojure.org/guides/learn/debugging)
- [Clojure 디버깅 도구 제안](https://github.com/clojure/tools.trace)