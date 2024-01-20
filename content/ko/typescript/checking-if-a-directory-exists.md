---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜하고 무엇을하는지? (What & Why?)

디렉토리 존재 여부를 확인하는 것은 파일 시스템에 특정 폴더가 존재하는지를 파악하는 과정입니다. 이것은 프로그래머가 파일을 읽거나 쓰기 전에 문제가 발생하지 않도록 하거나 또는 디렉토리를 처음 만들 때 중복을 방지하는 데 필요한 주요 도구입니다.

## 어떻게 하는가? (How to)

TypeScript에서 디렉토리 존재 여부를 확인하려면 Node.js의 `fs` 모듈을 사용하면 됩니다.
```TypeScript
import * as fs from 'fs';

const directory = './my-directory';

if (fs.existsSync(directory)) {
  console.log('The directory exists');
} else {
  console.log('The directory does not exist');
}
```
'./my-directory'가 존재하면 'The directory exists'라는 문장이 출력됩니다. 그렇지 않으면 'The directory does not exist' 라는 문구가 출력됩니다.

## 깊게 들어가기 (Deep Dive)

디렉토리 존재 여부 확인 기능은 파일 시스템 관련 작업이 필요할 때마다 매우 중요합니다. 이 과정이 없다면, 어플리케이션은 중복되는 디렉토리를 만들거나, 존재하지 않는 디렉토리에서 파일을 찾으려는 등의 문제를 겪을 수 있습니다.

이 행위의 대안은 'fs.stat' 또는 'fs.access'를 사용하는 것인데 이 방법들은 파일 또는 디렉토리의 정보를 제공하거나 존재여부를 확인하는 데 사용됩니다. 그러나 File System 모듈의 ‘fs.existsSync’는 가장 일반적이고 직관적인 방법입니다. 

## 참고 자료 (See Also)

다른 관련 자료들에 대해서는 아래 링크들을 참고해주세요:
- [Node.js fs Documentation](https://nodejs.org/api/fs.html)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [How to check if a file or directory exists in Node.js](https://stackoverflow.com/questions/17699599/how-to-check-if-a-file-or-directory-exists-in-node-js)