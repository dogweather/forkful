---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:51.634093-07:00
description: "\uBC29\uBC95: Clojure\uC5D0\uC11C\uB294 `*err*` \uC2A4\uD2B8\uB9BC\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC stderr\uC5D0 \uC4F8 \uC218 \uC788\uC2B5\uB2C8\uB2E4.\
  \ \uAE30\uBCF8 \uC608\uC81C\uB294 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.685015-06:00'
model: gpt-4-0125-preview
summary: "Clojure\uC5D0\uC11C\uB294 `*err*` \uC2A4\uD2B8\uB9BC\uC744 \uC0AC\uC6A9\uD558\
  \uC5EC stderr\uC5D0 \uC4F8 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 방법:
Clojure에서는 `*err*` 스트림을 사용하여 stderr에 쓸 수 있습니다. 기본 예제는 다음과 같습니다:

```clojure
(.write *err* "This is an error message.\n")
```

메시지를 쓴 후에는 스트림을 플러시하여 메시지가 즉시 출력되도록 해야 한다는 것에 유의하십시오:

```clojure
(flush)
```

stderr로의 샘플 출력:
```
This is an error message.
```

예외를 처리하는 경우, 스택 트레이스를 stderr에 출력하고 싶을 수 있습니다. 이를 위해 `printStackTrace`를 사용합니다:

```clojure
(try
  ;; 예외를 던질 수 있는 코드
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

더 구조화된 오류 로깅을 위해서는 `timbre`와 같은 타사 라이브러리를 stderr에 로그하기 위해 구성할 수 있습니다. 기본 설정 및 사용법은 다음과 같습니다:

먼저, `timbre`를 의존성에 추가합니다. 그런 다음 stderr를 사용하도록 구성합니다:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; stdout 로깅 비활성화
(timbre/set-config! [:appenders :spit :enabled?] false) ;; 파일 로깅 비활성화
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; 오류에 대해 stderr 활성화

(timbre/error "요청을 처리하는 동안 오류가 발생했습니다.")
```

이렇게 하면 오류 수준의 메시지가 stderr로 보내져 표준 애플리케이션 출력과 구별됩니다.
