---
title:                "Clojure: 문자열 대문자 변환"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Korean: 

제목: Clojure 프로그래밍을 위한 초보자 가이드

## 왜
자바 프로그래밍 언어를 새롭게 배우는 사람들은 종종 대소문자를 적절하게 구분하는 것에 어려움을 겪습니다. 그러므로 대소문자를 적절히 사용하기 위한 Clojure 함수를 배우기 전에 왜 이것이 중요한지 이해하는 것이 중요합니다.

## 대소문자 변환 방법
Clojure에서는 대소문자를 구분하는 데 유용한 두 가지 함수가 있습니다.

```Clojure
(def str "clojure programming")
(println (str/upper-case str))
;; 출력: CLOJURE PROGRAMMING

(println (str/lower-case str))
;; 출력: clojure programming
```

위의 예시에서 볼 수 있듯이, `upper-case` 함수는 문자열을 대문자로 변환하고, `lower-case` 함수는 소문자로 변환합니다.

## 깊이 파헤치기
다음 예시를 통해 대소문자 변환 함수를 완벽하게 이해해보겠습니다.

```Clojure
(def str "clojure is awesome")
(println (str/upper-case-first str))
;; 출력: Clojure is awesome

(println (str/capitalize str))
;; 출력: Clojure Is Awesome
```

`upper-case-first` 함수는 첫 글자만 대문자로 변환합니다. `capitalize` 함수는 각 단어의 첫 글자를 대문자로 변환합니다.

## 더 읽어보기
Korean: 
## 더 읽어보기
- [Clojure Documentation](https://clojure.org/)
- [Clojure Style Guide](https://guide.clojure.style/)
- [Clojurian Slack](https://clojurians.slack.com/)에서 Clojure 커뮤니티와 교류해보세요.