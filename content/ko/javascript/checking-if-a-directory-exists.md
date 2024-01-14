---
title:                "Javascript: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파일을 다루는 프로그래밍 작업을 하다 보면 종종 특정 디렉토리가 존재하는지 아닌지를 확인해야 할 때가 있습니다. 이러한 상황에서 디렉토리가 존재하는지 미리 체크하는 것은 코드의 안정성을 높이고 파일을 처리하는 과정에서 생길 수 있는 에러를 방지할 수 있습니다.

## 작업 방법

우선 주어진 경로가 디렉토리인지 확인하기 위해서는 `fs` (file system) 모듈을 이용해야 합니다. 아래는 `fs` 모듈을 이용하여 디렉토리가 존재하는지 확인하는 간단한 예제 코드입니다.

```Javascript
const fs = require('fs');

const path = './myDirectory'; // 특정 디렉토리 경로 지정
const directoryExists = fs.existsSync(path); // 디렉토리 존재 여부를 불리언 값으로 반환

console.log(directoryExists); // true or false 출력
```

위 코드에서 `fs.existsSync()` 메소드를 사용하여 경로가 존재하는지 미리 확인할 수 있습니다. 만약 해당 경로에 디렉토리가 존재하지 않는다면 `false`를 반환하고, 존재한다면 `true`를 반환합니다.

## 깊이있는 이야기

보다 디테일한 이야기를 들어보면 `fs.existsSync()` 메소드는 비동기적으로 파일을 처리하는 `fs.exists()` 메소드를 대체하고 있습니다. 과거 `fs.existsSync()` 메소드는 동기적으로 작동하며 스택을 차지하는 타이밍이 매우 컸기 때문에 자연스럽게 비동기 메소드가 대안으로 사용되게 되었습니다. 따라서 최신 버전의 Node.js에서는 `fs.existsSync()` 메소드가 deprecated 되었지만 여전히 많은 코드베이스에서 이용되고 있기 때문에 알아두는 것이 좋습니다. 

## 참고 자료

- [Node.js 공식 문서 - `fs` 모듈](https://nodejs.org/api/fs.html)
- [Node.js 공식 문서 - `fs.existsSync()` 메소드](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Node.js 공식 문서 - `fs.exists()` 메소드](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)