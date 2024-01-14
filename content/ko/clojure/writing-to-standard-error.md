---
title:    "Clojure: 표준 에러로 작성하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

표준 에러에 쓰는 것의 장점을 간략하게 설명합니다.

## 어떻게

"```Clojure
;; 예제 코드 블록 1
(println (str "더 많은 로그 정보를 얻고자 할 때 표준 에러에 쓰는 것이 유용합니다."))
;; 예제 코드 블록 2
(println (str "에러가 발생한 부분을 식별하고 디버깅에 도움이 됩니다."))
```"

## 심층 연구

표준 에러에 대해 더 깊이 있는 정보를 제공합니다.

### 표준 출력과 표준 에러의 차이

표준 출력은 프로그램에서 정상적으로 발생한 메시지를 출력할 때 사용됩니다. 하지만 표준 에러는 예외나 에러 메시지를 출력할 때 사용됩니다. 따라서 표준 에러는 프로그램에서 중요한 정보를 출력할 때 사용되며, 프로그램의 동작을 분석하고 디버깅하는 데 매우 유용합니다.

### 표준 에러의 캡처

Clojure에서는 *System/err* 함수를 사용하여 표준 에러를 캡처할 수 있습니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

(println "예제" (System/err "표준 에러에 쓰는 예제 코드"))

표준 에러는 표준 출력처럼 단순히 터미널에 출력되는 것이 아니라, 파일 등 다른 매체로 리디렉션하여 저장할 수도 있습니다.

## 더 알아보기

- [Clojure 공식 문서](https://clojure.org/reference/reading_errors)
- [표준 에러와 표준 출력의 차이](https://www.unixtutorial.org/understanding-stderr-in-unix)
- [Clojure에서 표준 에러를 캡처하는 방법](https://stackoverflow.com/questions/27319466/how-to-redirect-output-to-standard-error-in-clojure)