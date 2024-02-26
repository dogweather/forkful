---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:51.314135-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\
  \uB97C \uC5BB\uB294 \uAC83\uC740 \uB85C\uAE45, \uC774\uBCA4\uD2B8 \uD0C0\uC784\uC2A4\
  \uD0EC\uD551, \uC791\uC5C5 \uC2A4\uCF00\uC904\uB9C1 \uB4F1 \uB2E4\uC591\uD55C \uC774\
  \uC720\uB85C \uC911\uC694\uD569\uB2C8\uB2E4. JVM\uC0C1\uC758 Lisp \uBC29\uC5B8\uC778\
  \ Clojure\uC5D0\uC11C, \uC774 \uC791\uC5C5\uC740 Java \uC0C1\uD638 \uC6B4\uC6A9\uC131\
  \ \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\uC5EC \uD48D\uBD80\uD55C Java Date-Time\
  \ API\uC5D0 \uB300\uD55C \uAC04\uD3B8\uD55C \uC811\uADFC\uC744 \uAC00\uB2A5\uD558\
  \uAC8C\u2026"
lastmod: '2024-02-25T18:49:51.712602-07:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C\
  \ \uC5BB\uB294 \uAC83\uC740 \uB85C\uAE45, \uC774\uBCA4\uD2B8 \uD0C0\uC784\uC2A4\uD0EC\
  \uD551, \uC791\uC5C5 \uC2A4\uCF00\uC904\uB9C1 \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\
  \uB85C \uC911\uC694\uD569\uB2C8\uB2E4. JVM\uC0C1\uC758 Lisp \uBC29\uC5B8\uC778 Clojure\uC5D0\
  \uC11C, \uC774 \uC791\uC5C5\uC740 Java \uC0C1\uD638 \uC6B4\uC6A9\uC131 \uAE30\uB2A5\
  \uC744 \uD65C\uC6A9\uD558\uC5EC \uD48D\uBD80\uD55C Java Date-Time API\uC5D0 \uB300\
  \uD55C \uAC04\uD3B8\uD55C \uC811\uADFC\uC744 \uAC00\uB2A5\uD558\uAC8C\u2026"
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래밍에서 현재 날짜를 얻는 것은 로깅, 이벤트 타임스탬핑, 작업 스케줄링 등 다양한 이유로 중요합니다. JVM상의 Lisp 방언인 Clojure에서, 이 작업은 Java 상호 운용성 기능을 활용하여 풍부한 Java Date-Time API에 대한 간편한 접근을 가능하게 합니다.

## 어떻게 하나요:

### Java 상호 운용성 사용하기
Clojure의 Java와의 끊김없는 상호 운용성을 통해 Java Date-Time API를 직접 사용할 수 있습니다. 현재 날짜를 얻는 방법은 다음과 같습니다:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; 샘플 출력
(get-current-date) ; "2023-04-15"
```

### clj-time 라이브러리 사용하기
더 Clojure다운 해결책을 원한다면, Joda-Time을 래핑하는 `clj-time` 라이브러리를 선택할 수 있습니다. 그러나 대부분의 새 프로젝트에서는 내장된 Java 8 Date-Time API가 권장됩니다. 그럼에도 불구하고 `clj-time`을 선호하거나 필요로 한다면:

먼저, 프로젝트 의존성에 `clj-time`을 추가합니다. `project.clj`에 다음을 포함시키세요:

```clojure
[clj-time "0.15.2"]
```

그 다음, 현재 날짜를 얻기 위해 사용하세요:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; 샘플 출력
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

두 방법 모두 Clojure에서 현재 날짜를 빠르고 효과적으로 얻는 방법을 제공하며, 기반 Java 플랫폼의 강력함이나 Clojure 특유의 편리함을 활용합니다.
