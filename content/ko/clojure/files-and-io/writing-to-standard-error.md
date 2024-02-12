---
title:                "표준 에러에 쓰기"
aliases:
- /ko/clojure/writing-to-standard-error/
date:                  2024-02-03T19:32:51.634093-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
표준 오류(stderr)에 쓰기는 오류 메시지와 진단을 표준 출력(stdout)과 별도로 stderr 스트림으로 보내는 것을 말합니다. 프로그래머들은 이를 통해 일반 프로그램 출력과 오류 메시지를 구별하여 더 효과적인 디버깅과 로깅을 가능하게 합니다.

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
