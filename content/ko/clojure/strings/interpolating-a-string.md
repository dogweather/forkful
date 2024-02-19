---
aliases:
- /ko/clojure/interpolating-a-string/
date: 2024-01-20 17:50:48.952484-07:00
description: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774\uB780 \uBCC0\uC218\uB098 \uD45C\
  \uD604\uC2DD\uC758 \uAC12\uC744 \uBB38\uC790\uC5F4 \uC548\uC5D0 \uC9C1\uC811 \uC0BD\
  \uC785\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354 \uC77D\uAE30\
  \ \uC27D\uACE0 \uAC04\uACB0\uD558\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:05.668711
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uC774\uB780 \uBCC0\uC218\uB098 \uD45C\uD604\
  \uC2DD\uC758 \uAC12\uC744 \uBB38\uC790\uC5F4 \uC548\uC5D0 \uC9C1\uC811 \uC0BD\uC785\
  \uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uCF54\uB4DC\uB97C \uB354 \uC77D\uAE30 \uC27D\
  \uACE0 \uAC04\uACB0\uD558\uAC8C \uB9CC\uB4E4\uAE30 \uC704\uD574 \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC774 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 보간이란 변수나 표현식의 값을 문자열 안에 직접 삽입하는 것입니다. 코드를 더 읽기 쉽고 간결하게 만들기 위해 프로그래머들이 사용합니다.

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
