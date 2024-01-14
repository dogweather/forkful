---
title:                "TypeScript: 디렉토리가 존재하는지 확인하기"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 디렉토리가 존재하는지 확인하는 것인가?

디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 중요한 작업 중 하나입니다. 이를 통해 우리는 특정 작업을 수행하기 전에 디렉토리의 존재 여부를 확인할 수 있습니다. 디렉토리가 존재하지 않는 경우, 우리는 에러를 방지하고 코드를 더욱 효율적으로 만들 수 있습니다.

## 디렉토리가 존재하는지 확인하는 방법

```TypeScript
// 먼저, Node.js의 fs 모듈을 불러옵니다.
import * as fs from 'fs';

// 파라미터로 받은 디렉토리 경로를 검사합니다.
function checkIfDirectoryExists(directoryPath: string): boolean {
  // fs.existsSync() 메소드를 사용하여 디렉토리가 존재하는지 확인합니다.
  // 결과값으로 boolean 값을 반환합니다.
  if (fs.existsSync(directoryPath)) {
    return true;
  } else {
    return false;
  }
}

// 예시: test 폴더가 존재하는지 확인합니다.
const testFolderExists = checkIfDirectoryExists("./test");

// 결과값을 출력해봅니다.
console.log(testFolderExists); // true 
```

## 디렉토리가 존재하는지 확인의 심층 분석

실제로 디렉토리가 존재하는지 확인하는 것은 우리가 예상하는 것보다 조금 더 복잡할 수 있습니다. 예를 들어, 운영체제에 따라 파일 시스템의 구조가 다르기 때문입니다. 이를 해결하기 위해서는 다양한 방법이 존재합니다. Node.js의 fs 모듈을 사용하는 것은 그 중 하나입니다. 또한 다양한 라이브러리를 이용하여 우리의 목적에 맞게 디렉토리가 존재하는지 확인할 수 있습니다.

## See Also

- [Node.js fs 모듈 공식 문서](https://nodejs.org/api/fs.html)
- [npm 디렉토리 체크 라이브러리](https://www.npmjs.com/package/directory-exists)