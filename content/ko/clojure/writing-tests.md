---
title:    "Clojure: 테스트 작성하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

코드 테스트는 Clojure 프로그래머에게 중요한 일입니다. 그 이유는 코드를 작성하고 테스트하는 것이 만족스러운 코드를 만드는 데 도움이 되기 때문입니다.

## 어떻게

코드를 작성하기 전에 테스트 케이스를 만드는 것은 매우 중요합니다. 코드가 완성되면 테스트를 실행하고, 결과를 확인합니다. 이것은 코드를 수정하거나 개선할 때 유용한 기준이 됩니다. 아래는 간단한 예제입니다.

```Clojure
(defn sum [nums]
  (reduce + nums))

(sum [1 2 3]) ;; => 6
```

위의 코드에서 `sum` 함수를 테스트하기 위해 `sum` 함수를 호출하고 그 결과를 확인하는 테스트 케이스를 만들 수 있습니다.

```Clojure
(def result (sum [1 2 3]))
(assert (= 6 result)) ;; => 테스트 통과
(assert (= 5 result)) ;; => 테스트 실패
```

위의 예제에서 볼 수 있듯이 테스트를 쉽게 만들 수 있고, 코드의 예상되는 결과와 실제 결과를 비교하여 문제를 찾을 수 있습니다.

## 깊게 들어가기

코드 테스트는 코드를 안정적이고 효율적으로 작동하게 만들어줍니다. 또한 코드를 수정하거나 리팩토링할 때 실수를 방지해줍니다. Clojure에는 다양한 테스트 라이브러리가 있기 때문에, 여러분이 선호하는 라이브러리를 사용하여 코드 테스트를 진행할 수 있습니다.

테스트를 작성할 때는 테스트 케이스를 작성하는 것도 중요하지만, 이에 더해 테스트 대상 코드에서 나올 수 있는 모든 예외 상황도 고려해야 합니다. 이를 위해 `try/catch` 문을 사용하거나 `ex-info` 함수를 사용하여 에러 메시지를 만들 수 있습니다.

## 더 보기

- [Clojure 공식 문서](https://clojure.org/guides/testing)
- [clojure.test 라이브러리 문서](https://clojure.github.io/clojure/clojure.test-api.html)
- [Midje 라이브러리 문서](https://github.com/marick/Midje/wiki)
- [Clojure 테스트에 대한 더 많은 정보](https://www.clojure-toolbox.com/categories/testing)