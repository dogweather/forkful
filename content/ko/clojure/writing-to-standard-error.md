---
title:                "Clojure: 표준 에러에 쓰는 방법"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

왜 요코를 사용해서 표준 오류에 쓰는 것이 유용할까요? 이 글에서는 이유를 알려드리고 싶습니다.

# 어떻게

```Clojure
(println "안녕하세요?")
```
```Clojure
(> console.log "안녕하세요?")
```

콘솔의 로그에 메시지를 출력하는 것은 여러가지 이유로 유용합니다. 예를 들어, 디버깅을 위해서 해당 부분의 실행 상황을 살펴볼 수 있습니다. 이를테면, 로그를 이용하여 코드 흐름을 확인하거나 변수의 값을 확인할 수 있습니다. 클로저에서는 바로 표준 오류에 출력하는 함수인 *println*을 제공하고 있습니다. 아래는 콘솔과 비슷한 기능을 가진 *console.log* 함수를 만들어 살펴볼 것입니다. 여기서 *println*은 새로 구현된 *console.log* 함수의 별칭 역할을 합니다. 그리고 실행 결과를 보여드립니다.

```Clojure
(def console (java.util.logging.Logger/getLogger "console"))
(defn console.log [msg] (println msg))
```

```Clojure
(console "안녕하세요?")
```
```Clojure
안녕하세요?
```

코드의 실행 결과 마지막에는 *nil* 값이 표시됩니다. 이는 Clojure의 모든 함수가 값을 반환하기 때문입니다. 그렇다면 *println* 함수는 무엇을 반환할까요? 아래 예제를 통해 확인해보겠습니다.

```Clojure
(println "안녕하세요?")
```
```Clojure
안녕하세요?
nil
```

실행 결과를 보면, *println* 함수가 두 가지 값을 반환하는 것을 알 수 있습니다. 첫 번째는 넘겨받은 인자의 값을 출력한 후에 줄바꿈된 문자열입니다. 두 번째는 *nil* 값입니다. 이는 단순히 함수가 값을 반환하기 위한 규칙이며, *println* 함수의 주 목적은 출력하는 것이기 때문에 우리는 두 번째 값을 무시할 것입니다.

# 깊이 있는 알아보기

표준 오류의 출력은 특별한 용도를 위한 것이기 때문에, 이를 잘 활용하면 디버깅을 비롯한 여러가지 상황에서 유용하게 사용할 수 있습니다. 예를 들어, 로그를 이용하여 함수의 실행 상황을 파악하거나 예외 발생 시 해당 함수의 매개변수와 같은 디버깅 정보를 확인할 수 있습니다. 또한 시스템 간 연결 및 데이터 전달 과정에서 발생하는 오류를 파악하는 데에도 유용합니다. 이러한 이유로 표준 오류 내에 여러 가지 정보를 출력하여 앞서 언급한 상황들에서 유용하게 활용할 수 있습니다.

# 참고자료

- [Clojure 공식 문서](https://clojure.org/reference/REPL_and_More)
- [Clojure 튜토리얼](https://clojure.org/guides/getting_started)
- [간단한 디버깅 예제](https://medium.com/clojure-ko/%EB%A6%AC%EC%8A%A4%ED%8A%B8%EC%BD%94%EB%A7%98-clojure-%EA%B5%AC%ED%98%84%ED%95%8C%EC%9D%98-%EB%AA%A8%EB%93%A0-%EB%B0%A9%EB%B2%95-660c09075e11)