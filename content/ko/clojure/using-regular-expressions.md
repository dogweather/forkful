---
title:    "Clojure: 정규식 사용하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 코드를 보다 간결하고 빠르게 작성할 수 있기 때문입니다.

## 어떻게

```Clojure
;; 문자열에서 숫자만 추출하는 정규 표현식 예시
(def str "Number: 12345")
(re-find #"\d+" str)

;; 출력: "12345"
```

```Clojure
;; 이메일 형식 검사하는 정규 표현식 예시
(def email "example@email.com")
(if (re-find #".+@.+\..+" email)
    (println "올바른 이메일 형식입니다.")
    (println "이메일 형식이 올바르지 않습니다."))
    
;; 출력: "올바른 이메일 형식입니다."
```

```Clojure
;; 이메일 중복 검사하는 정규 표현식 예시
;; 중복되는 이메일이 있을 경우 true 반환
;; 중복되는 이메일이 없을 경우 false 반환
(def emails ["example1@email.com" 
             "example2@email.com"
             "example1@email.com"])
             
(defn check-duplicates
  [emails]
  (let [pattern #"(?i)([a-z0-9]+)@[a-z0-9]+\.[a-z]{2,3}" ;; (?i)는 대소문자를 구분하지 않음을 의미
        unique-emails (set emails)]
    (= (count unique-emails)
       (count (filter #(re-find pattern %) emails)))))
       
;; 출력: true       
```

## 심층 탐구

정규 표현식은 특정한 패턴이 있는 문자열을 처리할 때 매우 유용합니다. Clojure에서는 `re-find`와 `re-matches` 함수를 사용하여 정규 표현식을 적용할 수 있습니다. 또한, `re-seq` 함수를 사용하면 특정 패턴에 맞는 모든 부분 문자열을 추출할 수 있습니다.

또한, `re-matcher` 함수를 사용하면 정규 표현식을 별도의 객체로 만들어 사용할 수 있으며, `re-groups` 함수를 사용하여 그룹화된 문자열을 추출할 수 있습니다.

## 또 참고하세요

- [Clojure 정규 표현식 공식 문서](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-find)
- [Java 정규 표현식 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [정규 표현식 개념과 예시 (한국어)](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C_%ED%91%9C%ED%98%84%EC%8B%9D#Java%EB%A1%9C%EC%84%9C_%EC%A0%95%EA%B7%9C%ED%91%9C%ED%98%84%EC%8B%9D)