---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:02.785437-07:00
description: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB514\uB809\uD130\uB9AC\
  \uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\
  \uC77C \uC870\uC791 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC774\uBA70, \uC2A4\uD06C\
  \uB9BD\uD2B8\uAC00 \uC77D\uAE30\uB098 \uC4F0\uAE30 \uC804\uC5D0 \uB514\uB809\uD130\
  \uB9AC\uC758 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB3C4\uB85D \uD568\uC73C\uB85C\
  \uC368 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uACE0 \uD504\uB85C\uADF8\uB7A8 \uC2E4\
  \uD589\uC744 \uB354\uC6B1 \uC6D0\uD65C\uD558\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD2B9\
  \uD788 \uC0AC\uC6A9\uC790 \uC785\uB825\uC774\uB098 \uC678\uBD80 \uB370\uC774\uD130\
  \ \uC18C\uC2A4\uC5D0 \uAE30\uBC18\uD55C \uD30C\uC77C\uC774\uB098 \uB514\uB809\uD130\
  \uB9AC\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.811847-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB514\uB809\uD130\uB9AC\
  \uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\
  \uC77C \uC870\uC791 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC774\uBA70, \uC2A4\uD06C\
  \uB9BD\uD2B8\uAC00 \uC77D\uAE30\uB098 \uC4F0\uAE30 \uC804\uC5D0 \uB514\uB809\uD130\
  \uB9AC\uC758 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB3C4\uB85D \uD568\uC73C\uB85C\
  \uC368 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uACE0 \uD504\uB85C\uADF8\uB7A8 \uC2E4\
  \uD589\uC744 \uB354\uC6B1 \uC6D0\uD65C\uD558\uAC8C \uD574\uC90D\uB2C8\uB2E4."
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
