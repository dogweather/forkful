---
title:    "Clojure: 표준 오류 쓰기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 기록을 하나요?

이 자료는 Clojure를 사용하고 있는 개발자들에게 표준 에러를 기록하는 이유를 설명합니다. 표준 에러를 기록하는 것은 디버깅과 로깅에 매우 유용하며, 프로그램을 개발하거나 디버깅할 때 오류 메시지를 보다 쉽게 이해할 수 있도록 도와줍니다.

## 방법

표준 에러는 `(System/err)`를 사용하여 접근할 수 있습니다. 에러를 기록하기 위해서는 `print`나 `println`과 함께 사용할 수 있습니다.

```Clojure
(System/setErr (java.io.PrintStream. (java.io.FileOutputStream. "error.log")))
```

위의 코드는 단순히 에러 메시지를 출력하는 대신, 표준 에러를 `error.log` 파일에 기록하게 됩니다. 마찬가지로, `with-out-str`을 사용하여 표준 출력 대신 표준 에러를 기록할 수 있습니다.

```Clojure
(with-out-str
  (println "이것은 표준 출력입니다.")
  (System/err (println "이것은 표준 에러입니다.")))
```

위의 예시 코드에서는 표준 출력으로 `이것은 표준 출력입니다.`가 나오지만, 표준 에러로 `이것은 표준 에러입니다.`가 나오게 됩니다. 

## 더 깊이 들어가기

보통 `print`나 `println`을 사용하여 표준 에러를 기록할 때, 에러 메시지가 보기 좋지 않을 수 있습니다. 따라서 [Java의 `System.err`](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err--)를 참고하여 사용자 정의 메시지를 출력하거나, [Clojure의 `stack-trace-str`](https://clojuredocs.org/clojure.core/stack-trace-str)를 통해 자세한 스택 추적 정보를 출력할 수 있습니다.

## 참고 자료

- [Java `System.err` 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err--)
- [Clojure `stack-trace-str` 문서](https://clojuredocs.org/clojure.core/stack-trace-str)