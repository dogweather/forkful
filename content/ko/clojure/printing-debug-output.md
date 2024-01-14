---
title:                "Clojure: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜

프로그래밍을 하는 이유는 매우 다양하지만, 디버그 출력을 사용하는 이유는 매우 간단합니다. 이를 통해 우리는 코드가 어떻게 실행되고 있는지, 오류가 어디에서 발생하는지 등을 파악할 수 있습니다. 디버그 출력은 어떤 상황에서도 매우 유용한 도구입니다.

## 사용 방법

```Clojure
(defn add [a b]
  (let [sum (+ a b)]
    (println "Sum of" a "and" b "is" sum)
    sum))

(add 2 3)
```

위의 코드는 "Sum of 2 and 3 is 5"라는 디버그 출력을 생성합니다. 이와 같이 특정 변수의 값을 출력하거나, 함수가 실행되는 흐름을 파악하기 위해 미리 정해놓은 메시지를 출력할 수 있습니다.

또한 `clojure.pprint` 라이브러리를 사용하면 출력을 더 예쁘게 포맷할 수 있습니다.

```Clojure
(require '[clojure.pprint :refer [pprint]])

(defn print-list [lst]
  (pprint lst))

(print-list (range 5))
```

위의 코드는 다음과 같은 출력을 생성합니다.

```
(0 1 2 3 4)
```

## 깊게 파고들기

디버그 출력은 `println` 함수를 사용하면 어렵지 않게 할 수 있지만, 더 복잡한 상황에서는 `clojure.tools.logging` 라이브러리를 사용하는 것이 좋습니다. 이를 통해 다양한 수준의 로그를 추가하고, 로그의 형식 등을 더 세밀하게 설정할 수 있습니다.

또한 적절하게 로그를 추가하면 디버그 목적 외에도 운영 환경에서 유용한 정보를 얻을 수 있습니다.

# 참고자료

- [Clojure 디버깅: 유용한 팁과 툴](https://opensrcdesign.com/clojure-debugging-tips/): 디버그 출력 외에도 유용한 디버깅 팁들과 툴에 대한 소개입니다.
- [Debugging in Clojure](https://www.learn-clojure.com/?p=354): Clojure를 사용하는 개발자라면 반드시 알아야 할 디버깅 기법들을 예제와 함께 쉽게 설명한 글입니다.
- [Clojure Tools Loging](https://github.com/clojure/tools.logging): `clojure.tools.logging` 라이브러리 공식 깃허브 페이지입니다. 사용 방법과 예제 코드 등을 참고할 수 있습니다.