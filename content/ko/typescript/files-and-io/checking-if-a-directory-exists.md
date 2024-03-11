---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:53.558718-07:00
description: "TypeScript\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C \uC77D\uAE30\uB098\
  \ \uC4F0\uAE30 \uB4F1\uC758 \uD30C\uC77C \uAD00\uB9AC \uC791\uC5C5\uC5D0 \uD544\uC218\
  \uC801\uC774\uBA70, \uC720\uD6A8\uD55C \uB514\uB809\uD1A0\uB9AC\uC5D0\uC11C\uB9CC\
  \ \uC791\uC5C5\uC744 \uC218\uD589\uD568\uC73C\uB85C\uC368 \uC874\uC7AC\uD558\uC9C0\
  \ \uC54A\uB294 \uB514\uB809\uD1A0\uB9AC\uC5D0 \uC811\uADFC\uD558\uAC70\uB098 \uC870\
  \uC791\uC744 \uC2DC\uB3C4\uD560 \uB54C \uBC1C\uC0DD\uD558\uB294 \uC624\uB958\uB97C\
  \ \uD53C\uD558\uB294 \uB370 \uC911\uC694\uD55C \uC791\uC5C5\uC785\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:28.794855-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C \uC77D\uAE30\uB098\
  \ \uC4F0\uAE30 \uB4F1\uC758 \uD30C\uC77C \uAD00\uB9AC \uC791\uC5C5\uC5D0 \uD544\uC218\
  \uC801\uC774\uBA70, \uC720\uD6A8\uD55C \uB514\uB809\uD1A0\uB9AC\uC5D0\uC11C\uB9CC\
  \ \uC791\uC5C5\uC744 \uC218\uD589\uD568\uC73C\uB85C\uC368 \uC874\uC7AC\uD558\uC9C0\
  \ \uC54A\uB294 \uB514\uB809\uD1A0\uB9AC\uC5D0 \uC811\uADFC\uD558\uAC70\uB098 \uC870\
  \uC791\uC744 \uC2DC\uB3C4\uD560 \uB54C \uBC1C\uC0DD\uD558\uB294 \uC624\uB958\uB97C\
  \ \uD53C\uD558\uB294 \uB370 \uC911\uC694\uD55C \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
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
