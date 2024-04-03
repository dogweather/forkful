---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:02.785437-07:00
description: "\uBC29\uBC95: Node.js\uC5D0\uC11C\uB294 \uC790\uBC14\uC2A4\uD06C\uB9BD\
  \uD2B8 \uC790\uCCB4\uAC00 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uC9C1\uC811 \uC811\
  \uADFC\uD560 \uC218 \uC5C6\uAE30 \uB54C\uBB38\uC5D0, \uC774\uB7EC\uD55C \uC791\uC5C5\
  \uC744 \uC704\uD574 `fs` \uBAA8\uB4C8\uC774 \uC77C\uBC18\uC801\uC73C\uB85C \uC0AC\
  \uC6A9\uB429\uB2C8\uB2E4. \uC5EC\uAE30 `fs.existsSync()`\uB97C \uC0AC\uC6A9\uD574\
  \ \uB514\uB809\uD130\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\
  \uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.811847-06:00'
model: gpt-4-0125-preview
summary: "Node.js\uC5D0\uC11C\uB294 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8 \uC790\uCCB4\
  \uAC00 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uC9C1\uC811 \uC811\uADFC\uD560 \uC218\
  \ \uC5C6\uAE30 \uB54C\uBB38\uC5D0, \uC774\uB7EC\uD55C \uC791\uC5C5\uC744 \uC704\uD574\
  \ `fs` \uBAA8\uB4C8\uC774 \uC77C\uBC18\uC801\uC73C\uB85C \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
Node.js에서는 자바스크립트 자체가 파일 시스템에 직접 접근할 수 없기 때문에, 이러한 작업을 위해 `fs` 모듈이 일반적으로 사용됩니다. 여기 `fs.existsSync()`를 사용해 디렉터리가 존재하는지 확인하는 간단한 방법이 있습니다:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// 디렉터리가 존재하는지 확인
if (fs.existsSync(directoryPath)) {
  console.log('디렉터리가 존재합니다.');
} else {
  console.log('디렉터리가 존재하지 않습니다.');
}
```
**샘플 출력:**
```
디렉터리가 존재합니다.
```
또는 비차단(non-blocking) 비동기적 접근 방법으로, `fs.promises`와 `async/await`를 사용하십시오:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('디렉터리가 존재합니다.');
  } catch (error) {
    console.log('디렉터리가 존재하지 않습니다.');
  }
}

checkDirectory('./sample-directory');
```
**샘플 출력:**
```
디렉터리가 존재합니다.
```

파일 및 디렉터리 작업을 많이 사용하는 프로젝트의 경우, 기본 `fs` 모듈의 확장인 `fs-extra` 패키지가 편리한 추가 메소드를 제공합니다. 다음은 `fs-extra`를 사용해 동일한 작업을 수행하는 방법입니다:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// 디렉터리가 존재하는지 확인
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? '디렉터리가 존재합니다.' : '디렉터리가 존재하지 않습니다.'))
  .catch(err => console.error(err));
```
**샘플 출력:**
```
디렉터리가 존재합니다.
```

이 방법은 현대적 자바스크립트 관행과 원활하게 통합되면서 깔끔하고 읽기 쉬운 코드를 가능하게 합니다.
