---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:38.853164-07:00
description: "\uBC29\uBC95: \uD0C0\uC785\uC2A4\uD06C\uB9BD\uD2B8 \uC790\uCCB4\uB294\
  \ \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB85C \uCEF4\uD30C\uC77C\uB418\uC5B4 \uC804\
  \uD1B5\uC801\uC73C\uB85C \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uB300\uD55C \uC81C\
  \uD55C\uC801\uC778 \uC561\uC138\uC2A4\uB97C \uAC00\uC9C4 \uBE0C\uB77C\uC6B0\uC800\
  \uC5D0\uC11C \uC2E4\uD589\uB418\uAE30 \uB54C\uBB38\uC5D0 \uD30C\uC77C \uC791\uC5C5\
  \uC744 \uC9C1\uC811 \uCC98\uB9AC\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\
  \uB098 Node.js \uD658\uACBD\uC5D0\uC11C \uC0AC\uC6A9\uB420 \uB54C, `fs` \uBAA8\uB4C8\
  (\uD30C\uC77C \uC2DC\uC2A4\uD15C)\uC740 \uD30C\uC77C\uC744 \uC791\uC131\uD558\uB294\
  \ \uAE30\uB2A5\uC744\u2026"
lastmod: '2024-03-13T22:44:54.879574-06:00'
model: gpt-4-0125-preview
summary: "\uD0C0\uC785\uC2A4\uD06C\uB9BD\uD2B8 \uC790\uCCB4\uB294 \uC790\uBC14\uC2A4\
  \uD06C\uB9BD\uD2B8\uB85C \uCEF4\uD30C\uC77C\uB418\uC5B4 \uC804\uD1B5\uC801\uC73C\
  \uB85C \uD30C\uC77C \uC2DC\uC2A4\uD15C\uC5D0 \uB300\uD55C \uC81C\uD55C\uC801\uC778\
  \ \uC561\uC138\uC2A4\uB97C \uAC00\uC9C4 \uBE0C\uB77C\uC6B0\uC800\uC5D0\uC11C \uC2E4\
  \uD589\uB418\uAE30 \uB54C\uBB38\uC5D0 \uD30C\uC77C \uC791\uC5C5\uC744 \uC9C1\uC811\
  \ \uCC98\uB9AC\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
타입스크립트 자체는 자바스크립트로 컴파일되어 전통적으로 파일 시스템에 대한 제한적인 액세스를 가진 브라우저에서 실행되기 때문에 파일 작업을 직접 처리하지 않습니다. 그러나 Node.js 환경에서 사용될 때, `fs` 모듈(파일 시스템)은 파일을 작성하는 기능을 제공합니다.

### Node.js fs 모듈 사용하기
먼저, Node.js 환경에서 작업하고 있다는 것을 확인하세요. 그런 다음, `fs` 모듈을 사용하여 텍스트 파일을 작성합니다. 기본 예시는 다음과 같습니다:

```typescript
import * as fs from 'fs';

const data = 'Hello, world!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('파일이 저장되었습니다!');
});
```

이것은 `message.txt`에 비동기적으로 "Hello, world!"를 작성합니다. 파일이 존재하지 않으면 Node.js가 만들고, 존재하면 Node.js가 덮어씁니다.

동기적 파일 작성을 위해서는 `writeFileSync`을 사용하세요:

```typescript
import * as fs from 'fs';

const data = 'Hello again, world!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('파일이 저장되었습니다!');
} catch (err) {
    console.error(err);
}
```

### 인기 있는 타사 라이브러리 사용하기
네이티브 `fs` 모듈이 강력하지만 일부 개발자들은 추가적인 편리함과 기능성을 위해 타사 라이브러리를 사용하기를 선호합니다. `fs-extra`는 `fs`를 확장하고 파일 작업을 더 간단하게 만드는 인기 있는 선택입니다.

먼저, `fs-extra`를 설치해야 합니다:

```
npm install fs-extra
```

그런 다음 타입스크립트 파일에서 텍스트 내용을 작성하기 위해 사용할 수 있습니다:

```typescript
import * as fs from 'fs-extra';

const data = 'This is fs-extra!';
const filePath = './extraMessage.txt';

// async/await 사용하기
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('fs-extra로 파일이 저장되었습니다!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

이 코드 조각은 앞선 `fs` 예시와 동일한 작업을 수행하지만, 프로미스를 처리하는 더 깔끔한 문법을 제공하는 `fs-extra` 라이브러리를 활용합니다.
