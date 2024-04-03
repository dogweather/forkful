---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:53.558718-07:00
description: "\uBC29\uBC95: Node.js \uD658\uACBD\uC5D0\uC11C \uC2E4\uD589\uB420 \uB54C\
  \ TypeScript\uB294 `fs` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC \uB514\uB809\
  \uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD560 \uC218 \uC788\
  \uC73C\uBA70, \uC774 \uBAA8\uB4C8\uC740 \uB3D9\uAE30 \uD568\uC218\uC778 `existsSync()`\
  \ \uD568\uC218 \uB610\uB294 \uBE44\uB3D9\uAE30 \uD568\uC218\uC778 `access()` \uD568\
  \uC218\uC640 `constants.F_OK`\uB97C \uC870\uD569\uD558\uC5EC\u2026"
lastmod: '2024-03-13T22:44:54.873779-06:00'
model: gpt-4-0125-preview
summary: "Node.js \uD658\uACBD\uC5D0\uC11C \uC2E4\uD589\uB420 \uB54C TypeScript\uB294\
  \ `fs` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC \uB514\uB809\uD1A0\uB9AC\uAC00\
  \ \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD560 \uC218 \uC788\uC73C\uBA70, \uC774\
  \ \uBAA8\uB4C8\uC740 \uB3D9\uAE30 \uD568\uC218\uC778 `existsSync()` \uD568\uC218\
  \ \uB610\uB294 \uBE44\uB3D9\uAE30 \uD568\uC218\uC778 `access()` \uD568\uC218\uC640\
  \ `constants.F_OK`\uB97C \uC870\uD569\uD558\uC5EC \uC0AC\uC6A9\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
Node.js 환경에서 실행될 때 TypeScript는 `fs` 모듈을 사용하여 디렉토리가 존재하는지 확인할 수 있으며, 이 모듈은 동기 함수인 `existsSync()` 함수 또는 비동기 함수인 `access()` 함수와 `constants.F_OK`를 조합하여 사용할 수 있습니다.

### `fs.existsSync()` 사용하기:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('디렉토리가 존재합니다.');
} else {
  console.log('디렉토리가 존재하지 않습니다.');
}
```

### `fs.access()`와 `fs.constants.F_OK` 사용하기:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('디렉토리가 존재하지 않습니다.');
    return;
  }
  console.log('디렉토리가 존재합니다.');
});
```

디렉토리가 존재한다고 가정할 때 두 방법에 대한 **샘플 출력**:
```
디렉토리가 존재합니다.
```

존재하지 않는다면:
```
디렉토리가 존재하지 않습니다.
```

### 제3자 라이브러리 사용하기 - `fs-extra`:
`fs-extra`는 내장된 `fs` 모듈을 보완하고 더 편리한 함수들을 제공하는 인기 있는 제3자 라이브러리입니다.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`디렉토리가 존재합니다: ${exists}`);
});
```

디렉토리가 존재할 때의 **샘플 출력**:
```
디렉토리가 존재합니다: true
```

존재하지 않을 때:
```
디렉토리가 존재하지 않습니다: false
```
