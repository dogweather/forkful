---
title:                "디렉토리가 존재하는지 확인하기"
date:                  2024-02-03T19:08:53.558718-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
TypeScript에서 디렉토리가 존재하는지 확인하는 것은 파일 읽기나 쓰기 등의 파일 관리 작업에 필수적이며, 유효한 디렉토리에서만 작업을 수행함으로써 존재하지 않는 디렉토리에 접근하거나 조작을 시도할 때 발생하는 오류를 피하는 데 중요한 작업입니다.

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
