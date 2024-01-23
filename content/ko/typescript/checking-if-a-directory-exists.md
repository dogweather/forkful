---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:59:24.697690-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 필요한가?)
디렉토리가 존재하는지 확인하는 것은 파일 시스템에 특정 폴더가 있는지 검사하는 과정입니다. 프로그래머들은 파일을 읽거나 쓰기 전에 이 작업을 수행함으로써 에러를 피하고, 필요하다면 디렉토리를 생성합니다.

## How to: (방법)
```typescript
import * as fs from 'fs';
import { promisify } from 'util';

// 비동기적 방법 (async/await)
const existsAsync = promisify(fs.exists);

async function checkDirectory(directory: string) {
  const exists = await existsAsync(directory);
  console.log(`Directory ${directory} exists: ${exists}`);
}

// 동기적 방법
function checkDirectorySync(directory: string) {
  const exists = fs.existsSync(directory);
  console.log(`Directory ${directory} exists: ${exists}`);
}

// 샘플 사용 예시
checkDirectory('./data'); // 비동기적 방법 사용
checkDirectorySync('./data'); // 동기적 방법 사용
```

샘플 출력 결과:
```
Directory ./data exists: false
Directory ./data exists: false
```

## Deep Dive (심층 분석)
'fs.exists'는 Node.js가 초창기부터 제공하는 함수로, 디렉토리나 파일이 존재하는지 확인하는 데 사용됩니다. 그러나 이 방법은 현재는 권장되지 않습니다(‘fs.exists’의 사용은 공식적으로 노후화(deprecated)되었습니다). 더 나은 방법은 'fs.stat'나 'fs.access'을 사용하는 것이며, 이는 권한을 검사할 수도 있기 때문입니다.

또 다른 방법으로는, 써드파티 라이브러리를 사용하는 것인데, 예를 들어 'fs-extra'는 확장된 기능을 제공하며 사용하기 쉽습니다. TypeScript를 사용할 때는 써드파티 라이브러리에 대한 DefinitelyTyped의 타입 정의를 사용하면 퍼포먼스 및 타입 안정성을 확보할 수 있습니다.

## See Also (추가 정보)
- Node.js fs 모듈 문서: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- fs-extra 라이브러리: [https://www.npmjs.com/package/fs-extra](https://www.npmjs.com/package/fs-extra)
- TypeScript DefinitelyTyped: [https://definitelytyped.org/](https://definitelytyped.org/)

이렇게 'fs' 모듈을 이용해 디렉토리의 존재 유무를 확인하는 기본적인 방법들을 알아보았습니다.안정적이고 효율적인 코드를 작성하는 데 이 지식이 도움이 되길 바랍니다.
