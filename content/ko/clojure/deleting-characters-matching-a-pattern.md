---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Clojure: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 뭘하고 왜?: 
패턴과 일치하는 문자를 지우는 것은, 프로그래머들이 간단하고 빠르게 특정 문자열을 다룰 수 있도록 해주는 유용한 기술이다. 이 기술을 사용하면, 불필요한 문자를 쉽게 제거할 수 있어 코드를 간결하고 읽기 쉽게 만들 수 있다.

## 어떻게 하나?:
```Clojure
;Clojure에서 패턴과 일치하는 문자를 지우는 예제

(defn delete-char [str pattern] ;문자열과 패턴을 매개변수로 받는 함수 선언
  (apply str (remove #(= % pattern) str))) ;지우고 싶은 패턴이 있는지 확인하고 지우는 함수 적용

(delete-char "Hello World!" \o) ;"Hello Wld!" 출력
(delete-char "Clojure is awesome" \e) ;"Clojur is awsm" 출력

```

## 깊이 파고들기:
이 기술은 프로그래밍 언어의 역사적인 발전과 관련이 있다. 예전에는 문자열을 다루는 것이 더 어려웠고, 이러한 기능을 사용하지 못했기 때문에 코드가 길어지고 복잡해졌다. 하지만 지금은 이 기술을 사용하여 문자열 처리를 간결하고 쉽게 할 수 있다.

대안으로, 정규식을 사용하여 패턴과 일치하는 문자를 찾을 수도 있다. 하지만 정규식은 복잡하고 이해하기 어렵기 때문에, 패턴과 일치하는 문자를 지우는 것이 더 간단하고 직관적이다.

Clojure에서 이 기능은 ```remove``` 함수를 사용하여 구현된다. 이 함수는 모든 컬렉션에서 원하는 항목을 제거할 수 있다.

## 관련 자료:
- [Clojure Documentation](https://clojure.org/)
- [Regular Expressions in Clojure](https://clojure.org/guides/regular_expressions)