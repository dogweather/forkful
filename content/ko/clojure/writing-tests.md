---
title:                "테스트 작성하기"
html_title:           "Clojure: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

프로그래머는 테스트를 작성하는 이유가 있습니다. 테스트를 작성하는 것은 코드가 예상대로 작동하는지 확인하는 것입니다. 이렇게 하면 버그가 발생할 가능성을 줄일 수 있어 작업이 더 쉬워집니다.

# 하는 방법:

```Clojure
(deftest test-addition
  (is (= (+ 1 2) 3))
  (is (= (+ 5 10) 15)))
```

출력:

```
lein test

lein test user

Ran 1 tests containing 2 assertions.
0 failures, 0 errors
```

# 깊게 파고들기:

테스트를 작성하는 것은 코드를 안전하고 신뢰할 수 있게 만들어 줍니다. 예전에는 테스트를 수동으로 작성해야 했지만, Clojure에서는 테스트를 쉽게 작성할 수 있는 라이브러리가 제공됩니다. 또 다른 옵션으로는 REPL을 사용하여 직접 코드를 실행해 보는 것도 있습니다. Clojure는 기본적으로 테스트를 작성하기 쉬운 언어이기 때문에 이미 잘 알려진 테스트 프레임워크를 사용하지 않는 것이 좋습니다.

# 자료 참고:

- [ClojureDocs 테스트 라이브러리](https://clojuredocs.org/clojure.test)
- [Onyx 테스트 라이브러리](https://onyxplatform.org/guide/testing.html)
- [Clojure 테스트 설정하기](https://www.braveclojure.com/testing/)