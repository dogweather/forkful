---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 &, 왜?
문자열 내삽(interpolation)은 문자열 내부에 변수 값을 직접 삽입하는 것을 말한다. 프로그래머는 이것을 이용해 코드의 이해도를 높이고, 변수를 본래 형태로 설명하도록 도와준다.

## 이렇게 해보세요:
문자열 내삽을 어떻게 하는지 아래 Clojure 코드를 통해 살펴보도록 하겠습니다.

```Clojure
(def my-name "John Doe")
(def my-age 30)

(format "My name is %s and I am %d years old." my-name my-age)
```

위 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다: 

```Clojure
"My name is John Doe and I am 30 years old."
```

## 깊게 파보기
문자열 내삽은 long 또는 처음부터 만들어진 기교가 아니라 언어가 발전하면서 프로그래머들의 필요성에 의해 발전하였다.  

문자열 내삽에 대한 대안으로는 문자열 결합이 있지만 이는 읽기 어렵고 훨씬 더 복잡한 코드를 작성해야 하는 단점이 있다. 

Clojure에서는 자바의 `java.util.Formatter` 클래스를 기반으로 `format` 함수를 구현하여 문자열 내삽을 지원한다.

## 참고 
- [Clojure 공식 문서](https://clojure.org/)
- ['format' 함수에 대한 Clojure 공식 API 문서](https://clojuredocs.org/clojure.core/format)
- [StackOverflow: How to do string format in Clojure?](https://stackoverflow.com/questions/3709668/how-do-i-use-java-string-format-in-clojure)