---
title:                "TypeScript: 임시 파일 만들기"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜: 임시 파일을 생성하는 이유

임시 파일을 생성하는 것은 프로그래밍에서 중요한 부분입니다. 임시 파일은 일시적으로 사용되며, 주로 소프트웨어의 일부 기능을 수행하거나 데이터를 잠시 저장하는 용도로 사용됩니다.

## 작성방법

TypeScript에서 임시 파일을 생성하는 방법은 다음과 같습니다. 먼저, Node.js에 내장된 `fs` 모듈을 사용하여 파일 시스템 작업을 할 수 있도록 합니다.

```TypeScript
import { createWriteStream } from 'fs';

const temporaryFile = createWriteStream('temp.txt');
```

위의 코드는 `createWriteStream` 함수를 사용하여 `temp.txt`라는 파일을 생성합니다. 생성된 파일의 경로는 프로젝트의 루트 디렉토리에 저장됩니다. 이제 생성된 임시 파일에 데이터를 입력하고 파일을 닫아 보겠습니다.

```TypeScript
temporaryFile.write('Hello, world!', (err) => {
  if (err) throw err;
  temporaryFile.end();
});
```

위의 코드에서는 `write` 함수를 사용하여 `Hello, world!`라는 데이터를 파일에 쓰고, 데이터 입력이 완료되면 `end` 함수를 호출하여 파일을 닫습니다. 이제 `temp.txt` 파일을 확인하면 `Hello, world!`라는 내용이 입력된 것을 확인할 수 있습니다.

## 더 깊이 들어가보기

임시 파일을 생성할 때 중요한 점은 파일을 생성한 뒤 적절한 시점에 파일을 삭제하는 것입니다. 파일은 전체 프로그램 실행 중에 임시로만 사용되기 때문에, 불필요한 데이터 누적을 방지하기 위해 파일을 삭제하는 것이 중요합니다. 이를 위해 `fs` 모듈에 내장된 `unlink` 함수를 사용할 수 있습니다.

또 다른 중요한 점은 임시 파일을 생성할 때 마련한 경로가 다른 파일과 충돌하지 않도록 하는 것입니다. 이를 방지하기 위해 Node.js에는 유용한 `mkdtemp` 함수가 있습니다. `mkdtemp` 함수는 임시 디렉토리를 생성하는 함수로, 실제로는 무작위 문자열을 추가하여 충돌을 방지합니다.

## 관련 자료

- [Node.js 공식 문서: fs.createWriteStream](https://nodejs.org/api/fs.html#fs_fs_createwritestream_path_options)
- [TutorialsTeacher: Node.js 파일 시스템](https://www.tutorialsteacher.com/nodejs/nodejs-file-system) 

## 참고 자료

- [Node.js 공식 문서: fs.unlink](https://nodejs.org/api/fs.html#fs_fs_unlink_path_callback)
- [Node.js 공식 문서: fs.mkdtemp](https://nodejs.org/api/fs.html#fs_fs_mkdtemp_prefix_options_callback)