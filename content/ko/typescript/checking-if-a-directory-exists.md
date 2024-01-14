---
title:    "TypeScript: 디렉토리 존재 여부 확인하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파일 또는 폴더를 생성하기 전에 해당 경로에 이미 파일 또는 폴더가 있는지 확인하는 것은 중요합니다. 이를 통해 불필요한 파일 생성을 방지하고 오류를 줄일 수 있습니다.

## 방법

```TypeScript
// Node.js의 fs 모듈을 사용합니다.
import fs from 'fs';

// 폴더 경로를 지정합니다.
const directoryPath = './myFolder';

// 해당 경로에 폴더가 존재하는지 확인합니다.
if (fs.existsSync(directoryPath)) {
    console.log('폴더가 이미 존재합니다.');
} else {
    console.log('폴더가 존재하지 않습니다.');
}
```
### 출력:
```
폴더가 이미 존재합니다.
```

## 깊이 들어가기

폴더를 확인하는 더 다양한 방법이 있습니다. `fs.stat()` 함수를 사용하면 파일 또는 폴더의 상태를 더 자세하게 알 수 있습니다. 또한 폴더 내부의 파일 목록을 확인하려면 `fs.readdir()` 함수를 사용할 수 있습니다.

또한 파일 또는 폴더를 생성하기 전에 존재 여부를 확인할 때, 동기적인 방식으로 확인하는 것보다 비동기적인 방식이 더 효율적입니다. 이로 인해 프로그램이 멈추지 않고 다른 작업도 수행할 수 있습니다.

## 참고 자료
- [Node.js 공식 문서 - 파일 시스템 모듈 (fs)](https://nodejs.org/docs/latest-v14.x/api/fs.html)
- [TypeScript 핸드북 - 내장 모듈](https://www.typescriptlang.org/docs/handbook/modules.html#bundled-declarations)
- [fs 모듈 공식 문서](https://nodejs.org/api/fs.html)
- [fs.existsSync() 함수 공식 문서](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [fs.stat() 함수 공식 문서](https://nodejs.org/api/fs.html#fs_stat_path_options_callback)
- [fs.readdir() 함수 공식 문서](https://nodejs.org/api/fs.html#fs_readdir_path_options_callback)