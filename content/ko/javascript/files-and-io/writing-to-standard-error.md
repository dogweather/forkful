---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.998979-07:00
description: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uD45C\uC900 \uC624\
  \uB958(stderr)\uB85C \uC791\uC131\uD558\uB294 \uAC83\uC740 \uC624\uB958 \uBA54\uC2DC\
  \uC9C0\uB098 \uC911\uC694\uD55C \uC815\uBCF4\uB97C \uB85C\uAE45 \uBC0F \uB514\uBC84\
  \uAE45 \uBAA9\uC801\uC73C\uB85C \uD2B9\uC815 \uBD84\uB9AC\uB41C \uC2A4\uD2B8\uB9BC\
  \uC5D0 \uC9C0\uC2DC\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD2B9\uD788 Unix \uACC4\
  \uC5F4 \uD658\uACBD\uC5D0\uC11C \uC720\uC6A9\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC77C\uBC18 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC\
  \ \uC624\uB958 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBD84\uD568\uC73C\uB85C\uC368, \uCD9C\
  \uB825 \uAD00\uB9AC\uB97C \uB354\u2026"
lastmod: 2024-02-19 22:05:14.729324
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uD45C\uC900 \uC624\uB958\
  (stderr)\uB85C \uC791\uC131\uD558\uB294 \uAC83\uC740 \uC624\uB958 \uBA54\uC2DC\uC9C0\
  \uB098 \uC911\uC694\uD55C \uC815\uBCF4\uB97C \uB85C\uAE45 \uBC0F \uB514\uBC84\uAE45\
  \ \uBAA9\uC801\uC73C\uB85C \uD2B9\uC815 \uBD84\uB9AC\uB41C \uC2A4\uD2B8\uB9BC\uC5D0\
  \ \uC9C0\uC2DC\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD2B9\uD788 Unix \uACC4\uC5F4\
  \ \uD658\uACBD\uC5D0\uC11C \uC720\uC6A9\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC77C\uBC18 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC \uC624\
  \uB958 \uBA54\uC2DC\uC9C0\uB97C \uAD6C\uBD84\uD568\uC73C\uB85C\uC368, \uCD9C\uB825\
  \ \uAD00\uB9AC\uB97C \uB354\u2026"
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
자바스크립트에서 표준 오류(stderr)로 작성하는 것은 오류 메시지나 중요한 정보를 로깅 및 디버깅 목적으로 특정 분리된 스트림에 지시하는 것입니다. 특히 Unix 계열 환경에서 유용합니다. 프로그래머들은 일반 프로그램 출력과 오류 메시지를 구분함으로써, 출력 관리를 더 깔끔하게 하고 오류 모니터링을 더 쉽게하기 위해 이를 수행합니다.

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
