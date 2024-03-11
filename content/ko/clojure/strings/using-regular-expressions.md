---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:42.188703-07:00
description: "\uC815\uADDC \uD45C\uD604\uC2DD\uC740 \uC785\uB825 \uAC80\uC99D, \uAC80\
  \uC0C9, \uD14D\uC2A4\uD2B8 \uAD50\uCCB4\uC640 \uAC19\uC740 \uD14D\uC2A4\uD2B8 \uCC98\
  \uB9AC \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC778 \uD328\uD134 \uB9E4\uCE6D \uBC0F\
  \ \uB370\uC774\uD130 \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uB3C4\uAD6C\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBCF5\uC7A1\uD55C\
  \ \uBB38\uC790\uC5F4 \uD30C\uC2F1\uACFC \uB370\uC774\uD130 \uAC80\uC99D \uC791\uC5C5\
  \uC744 \uD6A8\uC728\uC801\uC774\uACE0 \uAC04\uACB0\uD558\uAC8C \uCC98\uB9AC\uD558\
  \uAE30 \uC704\uD574 \uC774\uB97C \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.561418-06:00'
model: gpt-4-0125-preview
summary: "\uC815\uADDC \uD45C\uD604\uC2DD\uC740 \uC785\uB825 \uAC80\uC99D, \uAC80\uC0C9\
  , \uD14D\uC2A4\uD2B8 \uAD50\uCCB4\uC640 \uAC19\uC740 \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\
  \ \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC778 \uD328\uD134 \uB9E4\uCE6D \uBC0F \uB370\
  \uC774\uD130 \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uB3C4\uAD6C\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBCF5\uC7A1\uD55C \uBB38\
  \uC790\uC5F4 \uD30C\uC2F1\uACFC \uB370\uC774\uD130 \uAC80\uC99D \uC791\uC5C5\uC744\
  \ \uD6A8\uC728\uC801\uC774\uACE0 \uAC04\uACB0\uD558\uAC8C \uCC98\uB9AC\uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uD569\uB2C8\
  \uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규 표현식은 입력 검증, 검색, 텍스트 교체와 같은 텍스트 처리 작업에 필수적인 패턴 매칭 및 데이터 조작을 위한 강력한 도구입니다. 프로그래머들은 복잡한 문자열 파싱과 데이터 검증 작업을 효율적이고 간결하게 처리하기 위해 이를 광범위하게 사용합니다.

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
