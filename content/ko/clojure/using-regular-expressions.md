---
title:                "정규식을 사용하는 방법"
html_title:           "Clojure: 정규식을 사용하는 방법"
simple_title:         "정규식을 사용하는 방법"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하려는 이유는 무엇이 있을까요? 

정규 표현식은 문자열을 검색, 추출, 대체하는 데 유용한 도구로서, 특히 데이터 처리와 텍스트 분석에 많이 사용됩니다. 또한, 다양한 언어와 플랫폼에서 사용할 수 있고 간단하고, 효율적인 방법으로 패턴 매칭을 수행할 수 있습니다.

## 사용 방법

정규 표현식은 `re-pattern` 함수와 `re-seq` 함수를 통해 생성하고, 문자열에서 매칭되는 부분을 찾을 수 있습니다.

```Clojure
(def input-string "Clojure is my favorite programming language")

(def regex (re-pattern "clojure"))

(re-seq regex input-string) ; => ("Clojure")
```

`re-find` 함수는 매칭되는 첫 번째 부분을 반환하며, `re-groups` 함수는 매칭된 하위 그룹을 반환합니다.

```Clojure
(def input-string "The quick brown fox jumps over the lazy dog")

(def regex (re-pattern "(\\w+) jumps over (\\w+)"))

(re-find regex input-string) ; => ["fox jumps over lazy" "fox" "lazy"]
(re-groups (re-find regex input-string)) ; => ["fox jumps over lazy" "fox" "lazy"]
```

매칭한 부분을 대체하려면 `re-gsub` 함수를 사용할 수 있습니다.

```Clojure
(def input-string "I love pizza and pasta")

(def regex (re-pattern "pizza|pasta"))

(re-gsub regex input-string "sushi") ; => "I love sushi and sushi"
```

## 깊이 파고들기

정규 표현식은 강력한 도구지만, 그만큼 학습 곡선이 까다로울 수 있습니다. 대부분의 경우 간단한 매칭을 위해서는 간단한 표현식만 필요하지만, 복잡한 매칭을 위해서는 정규 표현식 문법을 잘 이해해야 합니다.

범위, 캡처 그룹, 비슷한 패턴 등에 대해 조금 더 자세히 알아보려면 [이 문서](https://learnxinyminutes.com/docs/ko-kr/regex/)를 참고하세요.

## 관련 링크

- [Clojure 정규 표현식 문서](https://clojuredocs.org/clojure.core/re-find)
- [정규 표현식 이해하기 (번역)](https://brunch.co.kr/@mapthegod/37)
- [정규 표현식 테스트 사이트](https://regexr.com/)