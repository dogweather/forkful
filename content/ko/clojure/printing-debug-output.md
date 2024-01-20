---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 필요한가요?

디버그 출력은 프로그램이 실행되는 동안 일어나는 활동을 추적하고, 결과를 확인하기 위해 개발자가 사용하는 출력 메소드입니다. 이것은 오류를 찾고 해결하는 방법을 간소화하는데 궁극적으로 필요하다.

## 어떻게 사용하나요:

다음은 Clojure에서 디버그 출력을 생성하는 코드 예시입니다:

```clojure
(defn print-debug [msg]
  (println "디버그: " msg))

(print-debug "프로그램이 정상적으로 작동합니다.")
```
이것은 콘솔에 "디버그: 프로그램이 정상적으로 작동합니다."라는 메시지를 출력할 것입니다.

## 깊게 알아보기:

디버그 출력은 프로그램 디버깅의 오래된 기술이며, 대부분의 프로그래밍 언어에서 사용됩니다. 가능한 대안으로는 특별한 디버깅 도구 (예: 전용 디버거)를 사용하는 것이 있습니다. 그러나 디버그 출력의 간단함과 범용성 때문에 보편적으로 사용됩니다.

Clojure에서는, 간단한 `println` 함수를 사용하여 출력을 생성하는 것이 일반적입니다. 더 복잡한 디버깅에는 로깅 라이브러리가 종종 사용됩니다. 

## 참고 자료:

관련된 몇 가지 자료 리스트입니다:

1. Clojure 공식 문서 ([바로가기](https://clojure.org/guides/learn/functions))
2. “디버깅”에 대한 Clojure가이드 ([바로가기](https://clojure.org/guides/debugging))
3. Clojure의 `println` 함수에 대한 설명 ([바로가기](https://clojuredocs.org/clojure.core/println))