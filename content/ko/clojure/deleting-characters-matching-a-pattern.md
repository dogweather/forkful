---
title:    "Clojure: 패턴과 일치하는 문자 삭제"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 패턴과 일치하는 문자를 삭제하는 작업은 데이터를 정제하거나 특정 문자를 필터링하는 등 다양한 상황에서 유용합니다.

## 어떻게

Clojure에서는 정규표현식을 사용하여 문자열 내의 패턴과 일치하는 문자를 삭제할 수 있습니다. 다음은 `re-seq` 함수를 사용하여 문자열에서 숫자만 제거하는 예제 코드입니다.

```Clojure
(def numbers "35 apples, 45 oranges, 20 bananas")
(def pattern #"[0-9]+")

(re-seq pattern numbers)
```

이 코드를 실행하면 다음과 같은 출력이 나타납니다.

```Clojure 
("35" "45" "20")
```

위 예제에서는 숫자를 찾아서 리스트로 반환했지만, `re-seq`는 지정한 패턴과 일치하는 문자를 모두 삭제한 후 문자열 자체를 반환합니다. 따라서 `apply str` 함수를 사용하여 리스트를 문자열로 변환해야 합니다. 예를 들어, 문자열에서 모든 숫자를 제거하는 코드는 다음과 같습니다.

```Clojure
(def numbers "35 apples, 45 oranges, 20 bananas")
(def pattern #"[0-9]+")

(apply str (re-seq pattern numbers))
```

위 코드를 실행하면 아래와 같은 출력이 나타납니다.

```Clojure
" apples,  oranges,  bananas"
```

더욱 복잡한 패턴을 사용하여 문자를 삭제할 수도 있습니다. 예를 들어, 다음과 같은 코드를 사용하면 특정 문자들을 제거할 수 있습니다.

```Clojure
(def message "Hello, my name is Clojure!")

(def pattern #"[A-Za-z\s]" ;; 알파벳 문자와 공백 제거
(re-seq pattern message)

(def pattern #"\W" ;; 모든 알파벳, 숫자, 공백을 제외한 나머지 문자 제거
(apply str (re-seq pattern message))
```

위 코드들의 실행 결과는 아래와 같습니다.

```Clojure
("H" "e" "l" "l" "o" "," " " "m" "y" " " "n" "a" "m" "e" " " "i" "s" " " "C" "l" "o" "j" "u" "r" "e" "!")
"Hello my name is Clojure"
```

## 깊게 파헤치기

Clojure에서는 `re-seq` 외에도 문자열 패턴 매칭과 관련된 다양한 함수들이 존재합니다. 예를 들어, `re-find` 함수는 문자열 중에서 첫 번째로 일치하는 부분을 반환합니다. 이를 활용하면 다음과 같이 원하는 문자를 삭제하는 코드를 작성할 수 있습니다.

```Clojure
(def numbers "35 apples, 45 oranges, 20 bananas")
(def pattern #"\D+") ;; 숫자가 아닌 모든 문자 제거

(apply str (re-find pattern numbers))
```

`re-seq`와 달리 `re-find` 함수는 리스트가 아닌 일치하는 부분 자체를 반환하기 때문에, 따로 `apply str` 함수를 사용할 필요가 없습니다.

## 같이 보기

- [Clojure 공식 문서](https://clojure.org/guides/regex)
- [정규표현식의 기본 사용법](https://www.regular-expressions.info/quickstart.html)
- [Clojure에서 정규표현식을 사용하는 다른 방법](https://clojureverse.org/t/cozying-up-to-regex-redux/1571)