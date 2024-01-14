---
title:                "TypeScript: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하다 보면 종종 디렉토리가 존재하는지 확인해야 할 때가 있습니다. 이러한 경우, 명확한 이유로 인해 디렉토리가 이미 존재하는지 여부를 확인하는 것이 매우 중요합니다.

## 체크 방법

```TypeScript
import fs from 'fs';

const directoryPath = './my-directory';

try {
  // 지정된 경로의 디렉토리를 확인합니다.
  const isDirectoryExists = fs.existsSync(directoryPath);

  // 만약 디렉토리가 존재한다면 메시지를 출력합니다.
  if (isDirectoryExists) {
    console.log('디렉토리가 이미 존재합니다!');
  } else {
    console.log('디렉토리가 존재하지 않습니다.');
  }
} catch (err) {
  // 에러가 발생하면 해당 디렉토리가 존재하지 않는다고 출력합니다.
  console.log('디렉토리가 존재하지 않습니다.');
}
```

위의 예제 코드에서는 Node.js의 내장 모듈인 `fs`를 사용하여 디렉토리의 존재 여부를 확인합니다. 먼저 `fs.existsSync()` 메소드를 사용하여 지정된 경로의 디렉토리가 존재하는지 확인합니다. 그리고 `if-else` 구문을 사용하여 존재하는 경우와 그렇지 않은 경우에 대해 메시지를 출력합니다. 만약 디렉토리가 존재하지 않는다면 `try...catch` 구문을 사용하여 에러를 처리하고 해당 디렉토리가 존재하지 않는다는 메시지를 출력합니다.

## 디렉토리 체크에 대해 더 알아보기

위의 예제 코드에서 사용된 `fs.existsSync()` 메소드는 디렉토리가 아닌 파일의 존재 여부도 확인할 수 있습니다. 또한 `fs` 모듈의 다른 메소드들을 사용하면 디렉토리의 내용을 읽고 작성할 수도 있습니다. 디렉토리와 관련된 다양한 작업을 할 수 있는 방법은 여러 가지이며, 해당 기능의 활용 방법은 사용하는 프레임워크나 라이브러리에 따라 달라질 수 있습니다.

## 더 많은 자료 참조하기

디렉토리를 체크하는 방법에 대해 더 자세한 정보를 찾아보려면 아래의 링크들을 참조해 보세요.

- [Node.js 공식 문서 - 디렉토리 확인하기](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)
- [TypeScript 실전 강좌 - 디렉토리 관련 기능들](https://www.inflearn.com/course/%EC%96%B8%EB%8D%94%EC%8A%A4%ED%81%AC%EB%9D%BC%EC%9A%B0%EB%93%9C-%EC%8B%A4%EC%8A%B5)
- [MSDN - 디렉토리 관리 기능](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/file-system/how-to-manage-directories-in-visual-basic)