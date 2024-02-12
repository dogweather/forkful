---
title:                "텍스트 파일 쓰기"
aliases:
- /ko/typescript/writing-a-text-file/
date:                  2024-02-03T19:29:38.853164-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

타입스크립트에서 텍스트 파일을 작성하는 것은 데이터 영속성, 구성 또는 로그 생성을 위한 중요한 기술입니다. 프로그래머는 데이터 분석, 보고서 작성 또는 단순히 세션 간에 사용자 설정을 저장하기 위해 애플리케이션 메모리 외부에서 데이터를 저장하고 조작하는 작업을 종종 수행합니다.

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
