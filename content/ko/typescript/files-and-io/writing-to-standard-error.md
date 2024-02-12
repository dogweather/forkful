---
title:                "표준 에러에 쓰기"
aliases:
- /ko/typescript/writing-to-standard-error.md
date:                  2024-02-03T19:34:38.830702-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
TypeScript에서 표준 오류(stderr)에 쓰기는 환경의 오류 출력 스트림(예: node.js 또는 웹 브라우저의 콘솔)으로 직접 오류 메시지나 로그를 보내는 과정입니다. 이는 프로그램 데이터에 일반적으로 사용되는 표준 출력(stdout)과 간섭하지 않고 문제를 진단하는 데 필수적이며, 오류 처리와 로깅을 효율적이고 일관되게 관리할 수 있도록 보장합니다.

## 방법:
TypeScript는 JavaScript의 상위 집합으로써, stderr에 쓰기 위해 기본적인 JS 런타임 환경(Node.js 같은)에 의존합니다. 직접 실행하는 방법은 다음과 같습니다:

```typescript
console.error("This is an error message.");
```

stderr로의 샘플 출력:
```
This is an error message.
```

Node.js 환경에서는 보다 저수준 쓰기를 위해 `process.stderr.write()` 메소드를 사용할 수도 있습니다:

```typescript
process.stderr.write("Low level error message.\n");
```

stderr로의 샘플 출력:
```
Low level error message.
```

더 구조화된 오류 로깅을 위해서는 `winston`이나 `pino`와 같은 인기 있는 서드파티 라이브러리를 사용할 수 있습니다. `winston`을 사용하여 오류를 로깅하는 방법은 다음과 같습니다:

먼저 `winston`을 설치하십시오:

```bash
npm install winston
```

그런 다음 TypeScript 파일에서 사용하십시오:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Error logged using winston.');
```

이렇게 하면 오류가 콘솔과 `error.log`라는 이름의 파일에 모두 기록됩니다. 파일에 쓸 때는 파일 권한과 롤오버를 관리하여 디스크 공간 사용과 관련된 문제를 방지하는 것이 중요합니다.
