---
date: 2024-01-20 17:50:48.952484-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) Clojure \uC790\uCCB4\uB860 \uBB38\uC790\
  \uC5F4 \uBCF4\uAC04 \uAE30\uB2A5\uC744 \uC9C1\uC811 \uC81C\uACF5\uD558\uC9C0 \uC54A\
  \uC2B5\uB2C8\uB2E4. \uB2E4\uB978 \uC5B8\uC5B4, \uC608\uB97C \uB4E4\uC5B4 Ruby\uB098\
  \ Python\uC5D0\uC120 \uC774 \uAE30\uB2A5\uC774 \uB0B4\uC7A5\uB418\uC5B4 \uC788\uC8E0\
  . \uADF8\uB807\uC9C0\uB9CC Clojure \uCEE4\uBBA4\uB2C8\uD2F0\uC5D0\uC11C\uB294 \uC774\
  \uB97C \uC704\uD55C \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uB9CC\uB4E4\uC5C8\uACE0\
  , Clojure\uAC00 \uC81C\uACF5\uD558\uB294 \uD48D\uBD80\uD55C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.488893-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) Clojure \uC790\uCCB4\uB860 \uBB38\uC790\uC5F4 \uBCF4\
  \uAC04 \uAE30\uB2A5\uC744 \uC9C1\uC811 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (어떻게:)
```clojure
;; 사용자 정의 문자열 보간 예시
(defn interpolate [template & values]
  (reduce-kv (fn [s k v] (clojure.string/replace s (re-pattern (str "\\{" k "\\}")) v))
             template
             (into {} (map-indexed (fn [i v] [i v]) values))))

;; 사용 예
(def template "Hello, {0}! You have {1} new messages.")
(def output (interpolate template "Jinsoo" "5"))
(println output)
;; 출력: Hello, Jinsoo! You have 5 new messages.
```

## Deep Dive (심층 분석)
Clojure 자체론 문자열 보간 기능을 직접 제공하지 않습니다. 다른 언어, 예를 들어 Ruby나 Python에선 이 기능이 내장되어 있죠. 그렇지만 Clojure 커뮤니티에서는 이를 위한 라이브러리를 만들었고, Clojure가 제공하는 풍부한 문자열 처리 기능을 활용하여 사용자 정의 함수를 쉽게 작성할 수 있습니다.

예시에서 `interpolate` 함수는 템플릿 문자열과 임의의 값들을 받습니다. 모든 값은 인덱스 순서대로 맵핑되어 `{index}` 형태로 표시된 자리에 들어갑니다. `reduce-kv` 함수를 사용해 템플릿을 계속 업데이트하며 최종 문자열을 만들어냅니다.

다른 대안으로는 `clojure.core/str`, `format` 또는 `clojure.string/join`과 같은 Clojure의 기본 문자열 함수를 사용할 수도 있습니다. 그러나 이 방식들은 코드를 조금 더 복잡하게 만들 수 있으므로, 간결함이 중요할 때는 보통 문자열 보간 방식이 선호됩니다.

## See Also (참고 자료)
- 문자열 보간에 대한 커뮤니티 토론: [Clojure Google Group](https://groups.google.com/forum/#!topic/clojure)
- clojure.string 라이브러리 API 문서: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string)
