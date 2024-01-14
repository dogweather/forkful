---
title:                "Clojure: 패턴과 일치하는 문자 삭제하기"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
이 포스트에서는 사용자가 패턴과 일치하는 문자를 삭제하는 것이 어떻게 도움이 되는지에 대해 설명합니다.

## 어떻게
패턴에 일치하는 문자를 삭제하는 방법에 대한 예시 코드와 샘플 출력을 포함하여, Clojure 코드 블록 내에 들어갑니다.

```Clojure
(defn delete-matching-chars [pattern input]
  (str/replace input pattern ""))
;; 패턴과 일치하는 문자를 삭제하는 함수 정의

(def input "Clojure는 아주 멋진 프로그래밍 언어입니다.")
(delete-matching-chars #"아주 " input)
;; input 변수에서 "아주 " 문자열 삭제
```

출력: "Clojure는 멋진 프로그래밍 언어입니다."

## 딥 다이브
패턴에 맞게 삭제하는 것이 어떤 경우에 유용한지에 대한 더 자세한 정보를 제공합니다. 예를 들어, 특정 단어를 제거하고 싶을 때 이 방법을 사용할 수 있습니다. 또한 특정 패턴을 가진 문자열에서 패턴 매칭 문자를 삭제함으로써 데이터 정제에 유용한 기능으로 활용할 수 있습니다.

## 참고 자료
- Clojure 공식 홈페이지: https://clojure.org/
- Clojure 패턴 매칭 관련 함수 가이드: https://clojure.org/reference/java_interop
- Clojure 코드 예제와 설명: https://github.com/clojure
- Clojure를 이용한 데이터 정제 예제: https://www.baeldung.com/clojure-data-cleaning