---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체는 특정 패턴이나 문자열을 찾아 다른 텍스트로 변경하는 프로세스입니다. 프로그래머들은 코드의 일관성 향상, 버그 수정, 데이터 변환 등을 위해 이를 사용합니다.

## 어떻게 하는가: 

Clojure에서 문자열 교체는 매우 간단합니다. 아래의 기본적인 예제를 보세요:

```clojure
(replace {"A" "Apple", "B" "Banana"} "A B C")
```

이 코드는 "A B C" 문자열 중 "A" 를 "Apple"로, "B" 를 "Banana"로 교체하게 됩니다.

## 심화 학습

1) 역사적 맥락: Clojure는 2007년에 Rich Hickey에 의해 설계되었으며 Lisp 프로그래밍 언어 패밀리의 일부입니다. 이는 자바 가상 머신(JVM)에서 실행될 수 있도록 설계된 동시에 동적 서버 측 프로그래밍이 가능하도록 만들어졌습니다.

2) 대안: Clojure 외에도 Python, JavaScript, Ruby 등 다른 프로그래밍 언어들이 있다. 각기 다른 접근방식과 문법을 가지고 있지만 동일한 기능을 수행할 수 있다.

3) 실행 세부 정보: Clojure의 'replace' 함수는 키에서 값을 찾아 매핑하며, 검색 및 대체를 위한 매우 효율적인 도구입니다. 이는 근본적으로 맵 객체를 가져와 검색 패턴을 키로 사용하고 대체 텍스트를 값으로 사용합니다.

## 참고 자료

- Clojure 공식 웹사이트: [https://clojure.org/](https://clojure.org/)
- Clojure에 대한 더 깊은 통찰을 제공하는 블로그: [https://clojure.org/community/resources](https://clojure.org/community/resources)
- 친절한 Clojure: [https://aphyr.com/tags/Clojure-from-the-ground-up](https://aphyr.com/tags/Clojure-from-the-ground-up)