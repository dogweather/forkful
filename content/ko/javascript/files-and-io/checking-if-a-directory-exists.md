---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- /ko/javascript/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:02.785437-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
자바스크립트에서 디렉터리의 존재 여부를 확인하는 것은 파일 조작 작업에 필수적이며, 스크립트가 읽기나 쓰기 전에 디렉터리의 존재를 검증하도록 함으로써 오류를 방지하고 프로그램 실행을 더욱 원활하게 해줍니다. 특히 사용자 입력이나 외부 데이터 소스에 기반한 파일이나 디렉터리를 동적으로 처리하는 애플리케이션에서 중요합니다.

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
