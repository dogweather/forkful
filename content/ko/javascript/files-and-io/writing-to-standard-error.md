---
title:                "표준 에러에 쓰기"
aliases:
- /ko/javascript/writing-to-standard-error/
date:                  2024-02-03T19:33:48.998979-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
