---
title:    "Clojure: 난수 생성하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 왜

랜덤 숫자를 생성하는 것에 대해 궁금할 수 있습니다. 이 블로그 포스트에서는 이러한 재미있는 주제에 대해 알아보도록 하겠습니다!

## 사용 방법

Clojure에서 랜덤 숫자를 생성하는 것은 매우 간단합니다. `rand` 함수를 사용하여 0부터 1 사이의 난수를 생성할 수 있습니다. 예를 들어, 다음과 같이 작성할 수 있습니다:

```clojure
(rand)
```

결과는 다음과 같이 출력됩니다:

```
0.548193872
```

만약 범위를 지정하고 싶다면, `rand-int` 함수를 사용하면 됩니다. 예를 들어, 1부터 10 사이의 난수를 생성하려면 다음과 같이 작성할 수 있습니다:

```clojure
(rand-int 10)
```

결과는 다음과 같이 출력됩니다:

```
7
```

## 깊이 들어가기

Clojure에서는 `stateful`한 랜덤 숫자를 생성할 수도 있습니다. 예를 들어, `with-rand` 매크로를 사용하면, 랜덤 숫자 생성기의 상태를 유지하면서 여러 숫자를 생성할 수 있습니다. 예를 들어, 다음과 같이 작성할 수 있습니다:

```clojure
(with-rand
  (list (rand) (rand) (rand)))
```

결과는 다음과 같이 출력됩니다:

```
(0.127449064 0.38294138 0.73204572)
```

위의 예제에서는 `rand` 함수를 세 번 호출하고 있지만, 결과는 모두 다릅니다. 이는 `with-rand` 매크로가 랜덤 숫자 생성기의 상태를 유지하기 때문입니다.

## 더 알아보기

Clojure에서는 랜덤 숫자를 생성하는 방법에 대해 더 많은 것들이 있습니다. 이 포스트에서는 초보자를 대상으로 기본적인 내용만 다뤘지만, 더 깊이 들어가고 싶다면 다음 링크를 참조해보세요:

- [Clojure 공식 문서](https://clojure.org/reference/numbers#random_numbers)
- [Official ClojureScript Wiki](https://cljs.github.io/api/cljs.core/rand)
- [Brave Clojure 블로그 포스트](https://www.braveclojure.com/core-library-ref/)

## 같이 보기

- [Clojure에서 반복문 사용하기](https://github.com/colinknebl/colinknebl/blob/master/blogs/write-a-bad-code/bad-code-04-loops-for-clj.md)
- [Descargar Clojure](https://www.cognitect.com/clojure)