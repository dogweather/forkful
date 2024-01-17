---
title:                "표준 오류에 쓰는 방법"
html_title:           "Clojure: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

표준 에러에 쓰는 것은 간단히 말하면, 프로그래머가 오류 메시지나 디버깅 정보를 출력하는 것입니다. 이는 프로그램 실행 중에 오류를 파악하고 해결하기 위해서 사용됩니다.

## 어떻게 해야 하나요?

Clojure에서는 `System/err` 함수를 사용하여 표준 에러에 내용을 쓸 수 있습니다. 다음은 이 함수를 사용하는 예시 코드입니다:

```Clojure
(System/err "This is an error message.")
;; Output: This is an error message.
```

여러 줄의 메시지를 표준 에러에 쓰기 위해서는 `doto` 함수를 사용할 수 있습니다:

```Clojure
(doto (System/err) (println "This is the first line.")
       (println "This is the second line."))
;; Output: This is the first line.
;;         This is the second line.
```

## 깊이 파고들기

표준 에러를 쓰는 방식은 오래 전부터 컴퓨터 프로그래밍에서 사용되고 있습니다. 이전에는 파일로 내용을 저장하거나, 콘솔 커맨드를 사용하여 출력하였지만, 이제는 프로그래밍 언어에서 제공하는 함수를 사용하여 손쉽게 표준 에러를 출력할 수 있습니다. 또한, 대안으로는 `System/out` 함수를 사용하여 표준 출력에 내용을 쓰는 것이 있습니다.

표준 에러를 쓰는 것은 디버깅할 때 매우 유용합니다. 오류 메시지나 스택 트레이스 등 디버깅 정보를 표준 출력이 아닌 표준 에러에 쓰면, 어플리케이션 실행 시 출력되는 모든 내용을 캡처하여 더 편리하게 디버깅할 수 있습니다.

## 관련 자료

Clojure 공식 문서에서는 `System/err` 함수에 대한 더 자세한 내용을 확인할 수 있습니다. 또한 다른 프로그래밍 언어에서의 표준 에러 사용 방법도 비교하여 볼 수 있습니다.

Clojure 공식 문서: https://clojure.org/api/java.lang.System
표준 에러에 대한 더 많은 정보: https://en.wikipedia.org/wiki/Standard_error_(stream)