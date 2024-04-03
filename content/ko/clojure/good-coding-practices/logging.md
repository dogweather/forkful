---
date: 2024-01-26 01:03:25.223817-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00: Clojure\uB294 Java\uC758\
  \ \uB85C\uAE45 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\uC9C0\uB9CC, \uC880 \uB354\
  \ \uAD00\uC6A9\uC801\uC778 Clojure \uBC29\uC2DD\uC73C\uB85C \uC774\uB97C \uD65C\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. `clojure.tools.logging`\uC744 \uC0AC\uC6A9\
  \uD558\uB294 \uBC29\uBC95\uC744 \uC0B4\uD3B4\uBCF4\uACA0\uC2B5\uB2C8\uB2E4. \uC774\
  \uB294 \uC5EC\uB7EC \uB85C\uAE45 \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uAC04\uB2E8\
  \uD55C \uCD94\uC0C1\uD654\uB97C \uD1B5\uD574 \uC81C\uACF5\uD569\uB2C8\uB2E4:\u2026"
lastmod: '2024-03-13T22:44:54.670162-06:00'
model: gpt-4-1106-preview
summary: "Clojure\uB294 Java\uC758 \uB85C\uAE45 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\
  \uC9C0\uB9CC, \uC880 \uB354 \uAD00\uC6A9\uC801\uC778 Clojure \uBC29\uC2DD\uC73C\uB85C\
  \ \uC774\uB97C \uD65C\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 어떻게 사용하는가:
Clojure는 Java의 로깅 기능을 활용하지만, 좀 더 관용적인 Clojure 방식으로 이를 활용할 수 있습니다. `clojure.tools.logging`을 사용하는 방법을 살펴보겠습니다. 이는 여러 로깅 프레임워크를 간단한 추상화를 통해 제공합니다:

먼저, `clojure.tools.logging`과 `log4j` 같은 로깅 구현체에 대한 의존성을 `project.clj`에 추가하세요:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

이제, 몇 가지 메시지를 로깅해봅시다:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "집중된 계산을 시작합니다...")
  (Thread/sleep 3000) ; 장시간의 계산을 시뮬레이션 합니다
  (log/info "계산 완료. 답은 42입니다.")
  42)

(compute-answer-to-everything)
```
기본적으로, 출력에는 `DEBUG` 메시지가 보이지 않습니다. 로그 레벨은 일반적으로 `INFO`로 설정되어 있기 때문입니다:

```
INFO  [your-namespace] - 계산 완료. 답은 42입니다.
```

필요한 경우 `log4j.properties` 파일에서 로그 레벨과 애펜더들을 구성하여 더 상세한 출력을 얻을 수 있습니다.

## 깊은 탐구
Clojure의 `clojure.tools.logging`은 오랫동안 사용되어 왔고, Clojure 코드와 Java 로깅 세계 사이의 가교 역할을 합니다. 역사적으로, Java는 자체 로깅 API, `log4j`, `slf4j`, `logback`과 같은 여러 로깅 라이브러리로 여러 번 변화해왔습니다.

Clojure에서는 Java의 로깅 프레임워크를 직접 사용할 수 있지만, `clojure.tools.logging`은 클래스패스에서 발견한 어떤 로깅 프레임워크로든 위임을 하여, 특정 구현에 대한 강한 결합을 피할 수 있게 해줍니다. 이것은 여러분의 Clojure 코드를 더욱 휴대성 있고 모듈화되게 유지하는 데 도움을 줄 수 있습니다.

Clojure 생태계 내 `clojure.tools.logging`의 대안으로는 로그 회전, 필터링, 상자 로깅 등의 기능을 갖춘 순수한 Clojure 로깅 라이브러리인 `timbre` 등이 있습니다.

다중 스레드 환경에서 로깅을 특정하는 구현 세부사항은 Clojure와 같은 환경에서 매우 중요합니다. 여기서, 불변성과 부수효과 관리는 분명한 이점을 제공합니다. 로깅과 같은 부수효과는 성능 병목현상을 피하고 스레드 안전을 보장하기 위해 주의 깊게 처리되어야 하며, 대부분의 Java 로깅 프레임워크가 이미 이를 처리하고 있습니다.

마지막으로, 구조화된 로깅을 고려해보세요. 여기서, 로그들은 (JSON과 같이) 구조화된 데이터로 기록됩니다. 이것은 특히 대규모 분산 시스템을 다룰 때, 나중에 분석과 처리를 위해 극히 유용할 수 있습니다.

## 참고하기
더 많은 정보를 원한다면, 다음 자료들을 확인해보세요:

- Clojure Tools 로깅 문서: https://github.com/clojure/tools.logging
- Clojure 로깅 라이브러리인 Timbre: https://github.com/ptaoussanis/timbre
- Clojure에서 Log4J 구성: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- 고급 설정을 위한 Logback 매뉴얼: http://logback.qos.ch/manual/
- Clojure에서 구조화된 로깅에 대한 가이드: https://corfield.org/blog/2020/04/28/structured-logging/
