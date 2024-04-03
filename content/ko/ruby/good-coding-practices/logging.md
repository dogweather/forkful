---
date: 2024-01-26 01:09:06.989852-07:00
description: "\uC5B4\uB5BB\uAC8C: Ruby\uC5D0\uB294 \uC0AC\uC6A9\uD558\uAE30 \uB9E4\
  \uC6B0 \uC26C\uC6B4 \uB85C\uAE45\uC744 \uC704\uD55C \uB0B4\uC7A5 \uBAA8\uB4C8\uC778\
  \ `Logger`\uAC00 \uC788\uC2B5\uB2C8\uB2E4. \uC2DC\uC791\uD558\uB294 \uB370 \uB3C4\
  \uC6C0\uC774 \uB418\uB294 \uAC04\uB2E8\uD55C \uC608\uC81C\uB97C \uC18C\uAC1C\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.004930-06:00'
model: gpt-4-1106-preview
summary: "Ruby\uC5D0\uB294 \uC0AC\uC6A9\uD558\uAE30 \uB9E4\uC6B0 \uC26C\uC6B4 \uB85C\
  \uAE45\uC744 \uC704\uD55C \uB0B4\uC7A5 \uBAA8\uB4C8\uC778 `Logger`\uAC00 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 어떻게:
Ruby에는 사용하기 매우 쉬운 로깅을 위한 내장 모듈인 `Logger`가 있습니다. 시작하는 데 도움이 되는 간단한 예제를 소개합니다:

```ruby
require 'logger'

# STDOUT로 출력하는 Logger 생성
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# 로그 메시지 예제
logger.info("This is an info message")
logger.warn("This is a warning message")
logger.error("This is an error message")
```

위 스크립트를 실행하면 다음과 같은 내용이 출력됩니다:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : This is an info message
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : This is a warning message
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : This is an error message
```

로그 포맷과 레벨을 설정하여 불필요한 잡음을 걸러낼 수 있으며, 로그를 파일이나 외부 로깅 서비스와 같은 다른 출력 장치로 보낼 수 있습니다.

## 심층 분석
로깅은 프로그래밍의 수세기에 걸친 전통과도 같습니다. 역사적으로 로그는 `grep` 같은 도구로 수동으로 파싱되는 간단한 텍스트 파일이었습니다. 하지만 이 개념은 로그4j(Log4j), 리눅스(Linux)의 시스로그(Syslog), 혹은 클라우드 시대의 세마텍스트(Sematext), 로글리(Loggly)와 같은 견고한 로깅 프레임워크와 서비스들의 전체 생태계로 발전하였습니다.

Ruby의 `Logger`는 시작하기에 복잡함 없는 방법이지만, 더 많은 성능과 유연성이 필요한 경우에는 Lograge나 Semantic Logger와 같은 대안을 살펴보실 수 있습니다. 이러한 라이브러리는 Ruby 애플리케이션과 잘 작동하며, 로그 포매팅의 더 세부적인 제어를 제공합니다. 예를 들어 구조화된 로그(JSON 형식), 더 나은 성능, 그리고 다른 서비스와의 원활한 통합 등을 포함합니다.

각각의 Ruby 로깅 라이브러리는 자체적인 방법으로 작동하지만, 모두 메시지를 보내는 로거 인스턴스라는 개념을 중심으로 돌아갑니다. 로거는 설정된 레벨—DEBUG, INFO, WARN, ERROR, FATAL, UNKNOWN—을 바탕으로 이런 메시지를 처리하고, 이를 어떻게 할지 결정합니다: 출력하거나, 파일에 저장하거나, 네트워크를 통해 보내는 등.

## 참조
Ruby의 내장 로깅 모듈에 대한 심층 분석을 위해서는 공식 문서를 확인하세요:

더 발전된 로깅에 관심이 있거나 서드파티 젬들을 탐색하고 싶다면:
- [Lograge](https://github.com/roidrage/lograge)

일반적인 로깅 관행과 철학(루비 특정이 아님)에 대한 이러한 문서들은 영원한 참고자료입니다:
- [Google의 사이트 신뢰성 엔지니어링 책 - 16장: 과부하 처리](https://sre.google/sre-book/handling-overload/#log-messages)
- [12 Factor App - 로그](https://12factor.net/logs)
