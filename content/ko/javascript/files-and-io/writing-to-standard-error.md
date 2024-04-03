---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.998979-07:00
description: "\uBC29\uBC95: Node.js\uC5D0\uC11C stderr\uB85C \uC791\uC131\uD558\uB294\
  \ \uAC83\uC740 `console.error()` \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uAC70\
  \uB098 `process.stderr`\uC5D0 \uC9C1\uC811 \uC791\uC131\uD568\uC73C\uB85C\uC368\
  \ \uB2EC\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uB450\
  \ \uC811\uADFC \uBC29\uC2DD\uC744 \uBAA8\uB450 \uBCF4\uC5EC\uC8FC\uB294 \uC608\uC2DC\
  \uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.814784-06:00'
model: gpt-4-0125-preview
summary: "Node.js\uC5D0\uC11C stderr\uB85C \uC791\uC131\uD558\uB294 \uAC83\uC740 `console.error()`\
  \ \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 `process.stderr`\uC5D0\
  \ \uC9C1\uC811 \uC791\uC131\uD568\uC73C\uB85C\uC368 \uB2EC\uC131\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 방법:
Node.js에서 stderr로 작성하는 것은 `console.error()` 메서드를 사용하거나 `process.stderr`에 직접 작성함으로써 달성할 수 있습니다. 아래는 두 접근 방식을 모두 보여주는 예시입니다:

```javascript
// console.error() 사용하기
console.error('이것은 오류 메시지입니다.');

// process.stderr에 직접 작성하기
process.stderr.write('이것은 또 다른 오류 메시지입니다.\n');
```

두 방법 모두에 대한 샘플 출력은 stdout과 섞이지 않고 stderr 스트림에 나타날 것입니다:
```
이것은 오류 메시지입니다.
이것은 또 다른 오류 메시지입니다.
```

더 정교하거나 애플리케이션별 로깅을 위해, 많은 자바스크립트 프로그래머들은 `winston` 또는 `bunyan` 같은 제3자 라이브러리를 사용합니다. 여기 `winston`을 사용하는 간단한 예시가 있습니다:

먼저, npm을 통해 `winston`을 설치하세요:
```shell
npm install winston
```

그 다음, `winston`을 오류 로깅을 stderr에 하도록 설정하세요:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// 오류 메시지 로깅하기
logger.error('winston을 통해 로그된 오류.');
```

이 설정을 통해 `winston`을 사용하여 오류를 로그할 때, 표준 출력과 오류 출력 사이의 명확한 분리를 유지하도록 stderr로 지시하는 데 도움이 됩니다.
