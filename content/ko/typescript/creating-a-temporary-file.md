---
title:    "TypeScript: 임시 파일 만들기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 Temporary File을 생성하는가?

Temporary File을 생성하는 것은 파일을 일시적으로 만들고 사용한 후 삭제하는 것을 의미합니다. 이것은 메모리나 디스크 공간을 절약하는 데 도움이 됩니다. 예를 들어, 파일을 임시로 생성하여 작업을 수행하고 사용 후 삭제하면 더 이상 사용하지 않는 공간이 확보되기 때문에 메모리나 디스크 공간을 더 효율적으로 관리할 수 있습니다.

# 어떻게 하면 Temporary File을 생성할 수 있을까?

Temporary File을 생성하는 것은 TypeScript에서 아주 간단한 작업입니다. `fs` 모듈의 `mkdtemp` 함수를 사용하면 됩니다. 아래의 예제를 참고해 보세요.

```TypeScript
import * as fs from 'fs';

const tempDir = fs.mkdtempSync('./temp'); // temp 폴더에서 랜덤한 임시 디렉토리 생성

fs.writeFileSync(`${tempDir}/temp.txt`, 'Hello, world!'); // 임시 디렉토리에 파일 생성

console.log(fs.readdirSync(tempDir)); // 임시 디렉토리 내 파일 리스트 출력

fs.rmdirSync(tempDir); // 임시 디렉토리 삭제
```

위 코드를 실행하면 아래와 같은 결과가 나옵니다.

```
[ 'temp.txt' ] // 임시 디렉토리에 생성된 파일 리스트
```

# Deep Dive

`mkdtemp` 함수는 파라미터로 prefix를 받습니다. 이는 생성될 임시 디렉토리명의 접두사를 의미합니다. 또한 `fs` 모듈의 `mkdtemp` 함수는 비동기 함수이기 때문에 `fs.mkdtemp`를 사용하면 콜백 함수를 전달해야 합니다. 아래는 비동기 함수를 사용한 예제입니다.

```TypeScript
import * as fs from 'fs';

const tempDir: string = '';

fs.mkdtemp('./temp', (err, tempPath) => {
  if (err) throw err; // 에러 처리

  tempDir = tempPath; // 생성된 임시 디렉토리 경로 저장

  fs.writeFileSync(`${tempDir}/temp.txt`, 'Hello, world!'); // 임시 디렉토리에 파일 생성

  console.log(fs.readdirSync(tempDir)); // 임시 디렉토리 내 파일 리스트 출력

  fs.rmdirSync(tempDir); // 임시 디렉토리 삭제
});
```

결과는 동일합니다.

# See Also

- [Node.js File System 모듈](https://nodejs.org/api/fs.html)
- [Node.js 파일 시스템 경로 지정](https://www.geeksforgeeks.org/node-js-path-module/)
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)