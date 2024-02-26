---
date: 2024-01-26 01:08:57.950534-07:00
description: "\uB85C\uADF8\uB294 \uD504\uB85C\uADF8\uB7A8\uC774 \uC2E4\uD589\uB418\
  \uB294 \uB3D9\uC548 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uC774\uBCA4\uD2B8\uB97C\
  \ \uAE30\uB85D\uD558\uB294 \uACFC\uC815\uC73C\uB85C, \uC0AC\uD6C4 \uBD84\uC11D \uBC0F\
  \ \uC2E4\uC2DC\uAC04 \uBAA8\uB2C8\uD130\uB9C1\uC744 \uC704\uD55C \uC815\uBCF4\uC758\
  \ \uD754\uC801\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC774\uB7EC\uD55C \uB85C\uAE45\uC744 \uD1B5\uD574 \uBB38\uC81C \uD574\
  \uACB0, \uC131\uB2A5 \uBAA8\uB2C8\uD130\uB9C1, \uBCF4\uC548 \uBC0F \uBD84\uC11D\uC744\
  \ \uC704\uD55C \uC0AC\uC6A9\uC790\uC758 \uD589\uB3D9 \uCD94\uC801\uC5D0 \uB3C4\uC6C0\
  \uC744 \uBC1B\uAE30 \uB54C\uBB38\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:51.644008-07:00'
model: gpt-4-1106-preview
summary: "\uB85C\uADF8\uB294 \uD504\uB85C\uADF8\uB7A8\uC774 \uC2E4\uD589\uB418\uB294\
  \ \uB3D9\uC548 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uC774\uBCA4\uD2B8\uB97C \uAE30\
  \uB85D\uD558\uB294 \uACFC\uC815\uC73C\uB85C, \uC0AC\uD6C4 \uBD84\uC11D \uBC0F \uC2E4\
  \uC2DC\uAC04 \uBAA8\uB2C8\uD130\uB9C1\uC744 \uC704\uD55C \uC815\uBCF4\uC758 \uD754\
  \uC801\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC774\uB7EC\uD55C \uB85C\uAE45\uC744 \uD1B5\uD574 \uBB38\uC81C \uD574\uACB0\
  , \uC131\uB2A5 \uBAA8\uB2C8\uD130\uB9C1, \uBCF4\uC548 \uBC0F \uBD84\uC11D\uC744\
  \ \uC704\uD55C \uC0AC\uC6A9\uC790\uC758 \uD589\uB3D9 \uCD94\uC801\uC5D0 \uB3C4\uC6C0\
  \uC744 \uBC1B\uAE30 \uB54C\uBB38\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB85C\uAE45"
---

{{< edit_this_page >}}

## 무엇 & 왜?
로그는 프로그램이 실행되는 동안 애플리케이션 이벤트를 기록하는 과정으로, 사후 분석 및 실시간 모니터링을 위한 정보의 흔적을 제공합니다. 프로그래머들은 이러한 로깅을 통해 문제 해결, 성능 모니터링, 보안 및 분석을 위한 사용자의 행동 추적에 도움을 받기 때문에 사용합니다.

## 방법:
파이썬에는 로깅을 위한 내장 모듈이 있습니다. 다음은 기본 설정입니다:
```Python
import logging

# 로깅의 기본 설정
logging.basicConfig(level=logging.INFO)

# 로깅 메시지
logging.debug('디버그 메시지입니다')
logging.info('프로그램이 방금 한 작업에 대한 정보입니다')
logging.warning('경고 메시지입니다')
logging.error('오류가 발생했습니다')
logging.critical('프로그램이 회복할 수 없습니다!')
```
이 코드를 실행하면 다음과 같은 출력을 보게 됩니다(기본 수준이 WARNING이기 때문에 debug 및 info 메시지는 표시되지 않습니다):
```
WARNING:root:경고 메시지입니다
ERROR:root:오류가 발생했습니다
CRITICAL:root:프로그램이 회복할 수 없습니다!
```
콘솔 대신 파일에 로그를 작성하도록 설정할 수도 있습니다:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
이제 로그는 'app.log' 파일로 전송됩니다.

## 상세 설명
로그는 프로그래밍 초기부터 있었으며, 시스템 로그는 실제 데이터를 보유한 파일 외에 가장 오래된 지속적인 저장소 형태 중 하나입니다. 역사는 둘째 치고, 로깅의 주요 개념은 본질적으로 변하지 않았지만 도구는 발전했습니다.

파이썬의 `logging` 모듈은 매우 강력하고 유연합니다. 프로그래머가 다양한 로그 수준(DEBUG, INFO, WARNING, ERROR, CRITICAL)을 설정하여 로그를 분류하고 필터링하는 데 도움을 줍니다. 계층적 로거 시스템을 가지고 있어 로거 간에 부모-자식 관계를 가지고 체인을 통해 메시지를 전파할 수 있습니다.

대안으로는 Loguru 또는 structlog와 같은 타사 라이브러리가 있는데, 이들은 내장 로깅 모듈보다 강화된 기능과 더 간단한 인터페이스를 제공합니다. 더 예쁜 출력, 구조화된 데이터의 더 나은 직렬화, 로그 설정을 다루는 더 직관적인 방법을 제공할 수 있습니다.

구현과 관련하여 로깅을 설정할 때는 어플리케이션 시작 시 한 번만 설정하는 것이 중요합니다. 파이썬 로깅 모범 사례를 따르기 위해 모듈 수준에서 `logging.getLogger(__name__)`을 사용하는 것이 권장됩니다.

일반적인 상황에서 로깅은 어플리케이션의 성능에 심각한 영향을 주지 않아야 합니다. 하지만, 로그하는 내용에 주의해야 합니다: 과도하게 장황한 로깅, 특히 DEBUG 수준에서는 어플리케이션을 느리게 할 수 있고 로그 파일 저장소를 빠르게 채울 수 있습니다.

## 참조하세요
파이썬의 로깅 모듈에 대한 자세한 내용은 공식 파이썬 로깅 쿡북을 확인하십시오. 안에 훌륭한 예시와 모범 사례가 있습니다: https://docs.python.org/3/howto/logging-cookbook.html

로그를 더 정보적이고 분석하기 쉽게 만드는 구조화된 로깅에 대한 심층적인 이해를 위해, Loguru는 잘 문서화되어 있습니다: https://loguru.readthedocs.io

또한, 앱 로깅에 대한 현대적인 견해를 위해 12-요소 앱 방법론, 특히 로그 섹션을 살펴보는 것도 고려해볼 만합니다: https://12factor.net/logs
