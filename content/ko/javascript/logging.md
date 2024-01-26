---
title:                "로깅"
date:                  2024-01-26T01:08:00.331165-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/logging.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
간단히 말해서 로깅은 어플리케이션을 위한 일기장 같은 것입니다—소프트웨어가 실행되는 동안 발생하는 이벤트, 오류 및 기타 중요한 동작들을 기록합니다. 프로그래머들은 실시간으로 무슨 일이 일어나고 있는지를 이해하기 위해서뿐만 아니라, 디버깅, 감사, 그리고 성능 최적화를 위한 역사적 기록을 갖기 위해서 로깅을 합니다.

## 어떻게:
자바스크립트는 기본적으로 로그 메시지를 콘솔에 기록하는 간단한 방법을 제공합니다:

```javascript
console.log('이것은 콘솔에 기록될 것입니다');

// 출력:
// 이것은 콘솔에 기록될 것입니다
```

하지만 실제 앱은 단지 콘솔에 메시지를 출력하는 것 이상을 요구합니다. Winston이나 Pino 같은 라이브러리들을 도입해 효과적으로 로그를 관리할 수 있습니다:

```javascript
// Winston을 사용한 고급 로깅
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('안녕하세요, Winston으로 기록하는 로깅 이벤트입니다');
// 이 로그는 'combined.log'에 JSON 형식으로 기록됩니다
```

`combined.log` 샘플 출력:

```json
{"message":"안녕하세요, Winston으로 기록하는 로깅 이벤트입니다","level":"info"}
```

## 깊이 있게 탐구하기
로깅은 컴퓨팅 초기부터 필수적이었으며, 시스템 운영자들은 로그를 검토하여 시스템 성능을 이해하고 문제들을 진단하곤 했습니다. 현대 개발로 빠르게 전진하면서, 우리는 단순한 로그 파일에서 구조화되고 검색 가능한 로그 관리 시스템으로 변화했습니다.

자바스크립트에서 콘솔이나 파일 기반 로깅의 대안으로는 Loggly, Datadog 또는 ELK 스택(Elasticsearch, Logstash, Kibana)과 같은 클라우드 기반 로깅 서비스를 사용하는 것이 있습니다. 이러한 서비스들은 다양한 출처의 로그를 집계하고, 시각화 도구 및 고급 분석을 제공할 수 있습니다.

로깅을 구현할 때, 다음 사항들을 고려하세요:
- **세부 수준**: 디버그, 정보, 경고, 오류, 그리고 중대한 수준을 포함합니다.
- **성능**: 과도한 로깅은 어플리케이션 성능에 영향을 줄 수 있습니다.
- **보안**: 민감한 정보를 로깅하는 것에 대해 주의하세요.
- **포맷**: 구조화된 로그(예: JSON)는 검색과 파싱을 더 쉽게 만듭니다.
- **보존 정책**: 공간을 절약하기 위해 오래된 로그를 보관하거나 제거해야 합니다.

실용적인 로깅 전략은 무엇을 로그할지, 어디에 기록할지, 그리고 얼마나 오래 보관할지를 정의하면서 유용한 통찰력과 성능 및 프라이버시 고려 사항 간의 균형을 맞춥니다.

## 또한 보기
더 깊은 탐구를 위한 자료들을 확인해보세요:
- [Winston GitHub 저장소](https://github.com/winstonjs/winston): 자세한 사용법과 사용자 정의 전송을 위해서.
- [Pino - 매우 낮은 오버헤드를 가진 Node.js 로거](https://github.com/pinojs/pino): 가벼운 로깅 솔루션.
- [MDN 웹 문서: 콘솔](https://developer.mozilla.org/en-US/docs/Web/API/Console): 브라우저 기반 로깅 정보를 위해서.
- [Elastic ELK 스택](https://www.elastic.co/what-is/elk-stack): 로그 관리를 위한 강력한 삼총사.
- [12 Factor App 로깅](https://12factor.net/logs): 어플리케이션 로깅의 모범 사례.