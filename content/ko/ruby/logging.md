---
title:                "로깅"
date:                  2024-01-26T01:09:06.989852-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"

category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/logging.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
프로그래밍에서 로깅(logging)은 마치 애플리케이션의 일기를 기록하는 것과 같습니다. 이는 시스템적으로 이벤트, 메시지, 그리고 데이터 포인트를 기록하는 것으로써, 애플리케이션이 무엇을 하고 있으며 어떻게 동작하는지에 대한 통찰을 제공합니다. 개발자들은 디버깅, 애플리케이션 건강 모니터링, 그리고 잠재적 문제가 실제 문제로 크게 발전하기 전에 이에 대한 실마리를 얻기 위하여 로깅을 합니다.

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
