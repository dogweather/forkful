---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:42.188703-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Clojure\uB294 Lisp \uAC00\uBB38\uC5D0 \uBFCC\
  \uB9AC\uB97C \uB454 \uC5B8\uC5B4\uB2F5\uAC8C Java\uC758 \uC815\uADDC \uD45C\uD604\
  \uC2DD \uAE30\uB2A5\uACFC \uC6D0\uD65C\uD558\uAC8C \uC5F0\uB3D9\uB418\uB294 \uD48D\
  \uBD80\uD55C \uD568\uC218 \uC138\uD2B8\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\
  \uC74C\uC740 \uC774\uB97C \uD65C\uC6A9\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4\
  : \uBB38\uC790\uC5F4\uC774 \uD328\uD134\uACFC \uC77C\uCE58\uD558\uB294\uC9C0 \uAC80\
  \uC0AC\uD558\uB824\uBA74 `re-matches`\uB97C \uC0AC\uC6A9\uD558\uC138\uC694. \uC131\
  \uACF5 \uC2DC \uC804\uCCB4 \uC77C\uCE58\u2026"
lastmod: '2024-03-13T22:44:54.646114-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uB294 Lisp \uAC00\uBB38\uC5D0 \uBFCC\uB9AC\uB97C \uB454 \uC5B8\uC5B4\
  \uB2F5\uAC8C Java\uC758 \uC815\uADDC \uD45C\uD604\uC2DD \uAE30\uB2A5\uACFC \uC6D0\
  \uD65C\uD558\uAC8C \uC5F0\uB3D9\uB418\uB294 \uD48D\uBD80\uD55C \uD568\uC218 \uC138\
  \uD2B8\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 사용 방법:
Clojure는 Lisp 가문에 뿌리를 둔 언어답게 Java의 정규 표현식 기능과 원활하게 연동되는 풍부한 함수 세트를 제공합니다. 다음은 이를 활용하는 방법입니다:

### 기본 매칭
문자열이 패턴과 일치하는지 검사하려면 `re-matches`를 사용하세요. 성공 시 전체 일치 항목을 반환하고, 그렇지 않으면 `nil`을 반환합니다.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### 패턴 찾기
패턴의 첫 번째 발생을 찾으려면 `re-find` 함수가 가장 적합합니다:

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### 캡처 그룹
패턴에 괄호를 사용하여 `re-find`와 함께 그룹을 캡처하세요:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Area Code:" area "Code:" code))
;; 출력: Area Code: nil Code: 123
```

### 전역 검색 (모든 일치 항목 찾기)
Clojure는 일부 언어와 같은 내장 전역 검색 기능이 없습니다. 대신, `re-seq`을 사용해 모든 일치 항목의 게으른 시퀀스를 얻으세요:

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### 문자열 나누기
패턴을 기반으로 문자열을 나누려면 `clojure.string/split`을 사용하세요:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### 교체하기
문자열의 일치하는 부분을 패턴과 함께 `clojure.string/replace`로 교체하세요:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### 서드파티 라이브러리
Clojure의 내장 지원이 대부분의 경우에 충분하지만, 보다 복잡한 시나리오의 경우 `clojure.spec`을 이용한 로버스트 데이터 검증이나 웹 애플리케이션에서 정규식 기반 라우팅 및 입력 검증을 위한 `reagent`와 같은 라이브러리 사용을 고려하십시오.

```clojure
;; 이메일 검증을 위해 clojure.spec 사용 예시
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

정규 표현식은 강력하지만, 코드를 읽고 관리하기 어렵게 만들 수도 있다는 점을 기억하세요. 신중하게 사용하고 가능하면 더 간단한 문자열 조작 함수를 고려하세요.
