---
title:    "Clojure: 문자열 연결하기"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜

문자열을 연결(concatenate)하는 일은 프로그래머들이 자주 하는 작업 중 하나입니다. 특히 데이터베이스 레코드(record)를 읽거나 문서를 생성할 때 많이 사용됩니다. 이러한 이유로, Clojure 개발자라면 문자열 연결을 잘 다룰 줄 알아야 합니다.

## 어떻게

Clojure에서 문자열을 연결하는 가장 간단한 방법은 `str` 함수를 사용하는 것입니다. 이 함수는 인자로 받은 모든 값을 문자열로 변환한 뒤 이어붙여(concatenating) 리턴합니다.

```Clojure
(str "Hello" " " "World") ; => "Hello World"
```

또한 `str` 함수는 여러 가지 타입의 값에 대해서도 작동합니다.

```Clojure
(str 1 "+" 2 "=" 3) ; => "1+2=3"
(str true " is " false) ; => "true is false"
```

문자열이 아닌 값에 대해서도 `str` 함수를 사용할 수 있지만, 이 경우 해당 값이 문자열로 변환될 수 있는지 여부는 걱정하지 않아도 됩니다. `str` 함수는 이러한 값들을 자동으로 적절한 문자열로 변환해줍니다.

또 다른 방법으로는 `str` 함수 대신 Clojure의 문자열 보간(interpolation) 기능을 사용하는 것입니다. 이 방법은 문자열 내부에 변수나 식을 넣어서 원하는 형태의 문자열을 만들어낼 수 있습니다.

```Clojure
(def name "John")
(def age 25)
(def greeting (str "Hello, my name is " name ", and I am " age " years old."))

(str "My age is ${age} years old.") ; => "My name is John, and I am 25 years old."
```

## 깊이 파고들기

실제로 `str` 함수는 Clojure의 `fn` 함수를 이용해 간단하게 구현할 수 있습니다. 이를 위해서는 문자열을 저장할 빈 문자열을 정의하고, 모든 인자에 대해서 `conj` 함수를 사용해 이 문자열에 값을 붙여나가면 됩니다. 그리고 모든 인자를 순회한 뒤 생성된 문자열을 리턴하면 됩니다.

```Clojure
(defn my-str [& args]
  (let [result ""]
    (doseq [arg args]
      (conj result arg))
    result))

(my-str "Hello" " " "World") ; => "Hello World"
(my-str 1 "+" 2 "=" 3) ; => "1+2=3"
(my-str true " is " false) ; => "true is false"
```

하지만 이 방식은 `str` 함수가 사용하는 Clojure의 내부 함수들을 모두 이해하고 있어야 올바르게 구현할 수 있습니다. 따라서 일반적인 프로그래밍 상황에서는 `str` 함수를 사용하는 것이 더 효율적일 것입니다.

# 참고 자료

- [ClojureDocs: str](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs: fn](https://clojuredocs.org/clojure.core/fn)
- [Clojure for the Brave and True: Strings](https://www.braveclojure.com/do-things/#Strings)