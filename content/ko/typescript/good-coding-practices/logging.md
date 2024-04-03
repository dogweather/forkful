---
date: 2024-01-26 01:09:42.958297-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694? TypeScript\uC5D0\uC11C\
  \uB294 \uCF58\uC194 \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uAE30\uBCF8\
  \uC801\uC778 \uB85C\uAE45\uC744 \uC27D\uAC8C \uAD6C\uD604\uD560 \uC218 \uC788\uAC70\
  \uB098 'winston'\uC774\uB098 'pino' \uAC19\uC740 \uB354 \uACE0\uAE09 \uB85C\uAE45\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD569\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uB2E4\uC74C\uC740 `console.log`\uB97C \uC0AC\uC6A9\uD55C \uAE30\uBCF8\uC801\
  \uC778 \uC608\uC2DC\uC640 `winston`\uC744\u2026"
lastmod: '2024-03-13T22:44:54.862379-06:00'
model: gpt-4-1106-preview
summary: "TypeScript\uC5D0\uC11C\uB294 \uCF58\uC194 \uBA54\uC18C\uB4DC\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uAE30\uBCF8\uC801\uC778 \uB85C\uAE45\uC744 \uC27D\uAC8C \uAD6C\
  \uD604\uD560 \uC218 \uC788\uAC70\uB098 'winston'\uC774\uB098 'pino' \uAC19\uC740\
  \ \uB354 \uACE0\uAE09 \uB85C\uAE45 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD569\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 어떻게 사용하나요?
TypeScript에서는 콘솔 메소드를 사용하여 기본적인 로깅을 쉽게 구현할 수 있거나 'winston'이나 'pino' 같은 더 고급 로깅 라이브러리를 통합할 수 있습니다. 다음은 `console.log`를 사용한 기본적인 예시와 `winston`을 사용한 보다 고급 예시입니다.

```TypeScript
// 기본 콘솔 로깅
console.log('Info: 애플리케이션 시작 중...');
console.error('Error: 데이터 검색 불가.');

// 샘플 출력
// Info: 애플리케이션 시작 중...
// Error: 데이터 검색 불가.
```

더 견고한 로깅을 위해 `winston`을 설정해 봅시다:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('서버가 시작되었습니다!');
logger.warn('디스크 공간 부족 경고.');
logger.error('데이터베이스에 연결하지 못했습니다.');

// combined.log의 샘플 출력
// 2023-01-20 14:42:07 info: 서버가 시작되었습니다!
// 2023-01-20 14:42:09 warn: 디스크 공간 부족 경고.
// 2023-01-20 14:42:12 error: 데이터베이스에 연결하지 못했습니다.
```

## 깊이 알아보기:
컴퓨팅의 맥락에서 로깅이라는 개념은 프로그래밍의 초기 시대로 거슬러 올라가며, '로그북'이라는 항해 기록 보관 시스템에서 유래되었습니다. 역사적으로 프로그램 이벤트는 종종 메인프레임 시대 특히, 물리적인 출력물이나 터미널 출력으로 기록되었습니다.

오늘날로 넘어와서는 간단한 텍스트 파일부터 복잡한 로그 관리 시스템에 이르기까지 다양한 로깅 요구를 충족하는 도구와 라이브러리를 다수 이용할 수 있습니다. `winston`의 대안으로는 고성능을 자랑하는 `pino`, JSON 기반의 `Bunyan` 등이 있습니다. Node.js를 작업할 때 로깅 라이브러리는 종종 로그를 다른 목적지로 분류하는 스트림 메커니즘을 제공하며, 로그 로테이션과 맞춤형 포매터화를 지원합니다.

구현 측면에서, 로그 메시지는 보통 타임스탬프, 심각성 수준(예: info, warn, error), 그리고 실제 메시지를 포함합니다. 좋은 로깅 관행은 로그 수준을 적절하게 분류하고, 로그에서 민감한 데이터를 피하며, 고처리량 애플리케이션에서 성능 영향을 고려하는 것을 권장합니다.

## 또한 참조하십시오:
- [Winston - 거의 모든 것을 위한 로거](https://www.npmjs.com/package/winston)
- [Pino - 매우 낮은 오버헤드의 Node.js 로거](https://www.npmjs.com/package/pino)
- [Node.js 로깅 최고의 실천법](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [12팩터 앱 - 로그](https://12factor.net/logs)
