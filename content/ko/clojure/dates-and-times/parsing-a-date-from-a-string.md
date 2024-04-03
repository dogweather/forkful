---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:57.430297-07:00
description: "\uD074\uB85C\uC800\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130\
  \ \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04\uC758 \uD14D\uC2A4\uD2B8 \uD45C\uD604\uC744 \uB354 \uC0AC\uC6A9\uD558\uAE30\
  \ \uC88B\uC740 \uD615\uD0DC(\uC608: \uD074\uB85C\uC800\uC758 DateTime \uAC1D\uCCB4\
  )\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4\
  . \uC774 \uACFC\uC815\uC740 \uB370\uC774\uD130 \uCC98\uB9AC, \uB85C\uAE45 \uB610\
  \uB294 \uC2DC\uAC04 \uB370\uC774\uD130\uB97C \uC870\uC791\uD558\uB294 \uBAA8\uB4E0\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0 \uC788\uC5B4 \uAE30\uBCF8\uC801\uC774\
  \uBA70, \uD504\uB85C\uADF8\uB798\uBA38\uAC00\u2026"
lastmod: '2024-03-13T22:44:54.674983-06:00'
model: gpt-4-0125-preview
summary: "\uD074\uB85C\uC800\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\
  \uC9DC\uB97C \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\
  \uC758 \uD14D\uC2A4\uD2B8 \uD45C\uD604\uC744 \uB354 \uC0AC\uC6A9\uD558\uAE30 \uC88B\
  \uC740 \uD615\uD0DC(\uC608: \uD074\uB85C\uC800\uC758 DateTime \uAC1D\uCCB4)\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uAC83\uC5D0 \uB300\uD55C \uAC83\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 무엇 & 왜?
클로저에서 문자열로부터 날짜를 파싱하는 것은 날짜와 시간의 텍스트 표현을 더 사용하기 좋은 형태(예: 클로저의 DateTime 객체)로 변환하는 것에 대한 것입니다. 이 과정은 데이터 처리, 로깅 또는 시간 데이터를 조작하는 모든 애플리케이션에 있어 기본적이며, 프로그래머가 날짜에 대해 효율적으로 연산, 비교 또는 조작 작업을 수행할 수 있게 합니다.

## 방법:
클로저는 JVM 언어이기 때문에 자바의 날짜와 시간 라이브러리를 직접 사용할 수 있습니다. 내장된 자바 상호운용성으로 시작한 다음, 더 관용적인 클로저 솔루션을 위해 인기 있는 타사 라이브러리인 clj-time을 사용하는 방법을 탐구해보겠습니다.

### 자바 상호운용 사용하기
클로저는 자바의 `java.time.LocalDate`를 직접 활용하여 문자열에서 날짜를 파싱할 수 있습니다:
```clojure
(require '[clojure.java.io :as io])

; 자바 상호운용을 사용한 날짜 파싱
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; 출력: 2023-04-01
```

### clj-time 사용하기
날짜와 시간을 다루기 위한 더 관용적인 클로저 라이브러리는 `clj-time`입니다. 이것은 날짜와 시간 연산을 위한 포괄적인 라이브러리인 Joda-Time을 래핑합니다. 우선, 프로젝트의 의존성에 `clj-time`을 추가할 필요가 있습니다. 다음은 `clj-time`을 사용하여 날짜 문자열을 파싱하는 방법입니다:

```clojure
; 프로젝트.clj의 :dependencies 아래에 [clj-time "0.15.2"]를 추가하세요

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; 포맷터 정의하기
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; 출력: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

이 예시들은 기본적인 날짜 파싱을 보여줍니다. 두 방법 모두 유용하지만, `clj-time`은 복잡한 요구 사항을 위한 추가 기능과 함께 더 클로저 중심적인 접근 방식을 제공할 수 있습니다.
