---
title:                "임시 파일 생성하기"
html_title:           "TypeScript: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜 temporary 파일을 만들까요?

temporary 파일은 프로그래밍 프로세스에서 매우 중요한 역할을 합니다. 이 파일은 우리가 다른 작업을 수행하는 동안 일시적으로 데이터를 저장하기 위해 사용됩니다. 따라서 이 파일을 만드는 것은 우리가 하고 있는 작업에 대한 안정성과 성능을 향상시키는 데 도움이 됩니다.

## 만드는 방법

```TypeScript
// temporary 파일을 만드는 예제 코드
import fs from 'fs';

const data = '이것은 temporary 파일에 저장될 데이터입니다.';
const filePath = './temp/tempFile.txt';

// temporary 파일 생성
fs.writeFile(filePath, data, (err) => { 
    if (err) throw err; 
    console.log('temporary 파일이 생성되었습니다.');
});
```

위의 예제 코드를 실행하면 `tempFile.txt`라는 이름의 temporary 파일이 생성됩니다. 파일이 생성되면 해당 파일의 경로에 지정된 데이터가 저장됩니다. 이렇듯 temporary 파일을 생성하면 우리는 다른 파일이나 프로세스를 변경하지 않고도 필요한 데이터를 일시적으로 저장할 수 있습니다.

## 더 깊이 들어가보기

temporary 파일을 만들 때의 세부 사항 중 하나는 사용할 파일의 경로와 파일 이름을 결정하는 것입니다. 대부분의 경우 이 파일들은 운영 체제의 임시 디렉토리에 저장됩니다. 이는 운영 체제와 하드웨어의 차이로 인해 프로그래밍 프로세스 중에 파일 경로에 문제가 발생할 수 있기 때문입니다. 따라서 우리는 운영 체제의 임시 디렉토리를 참조하는 함수를 사용해 파일의 경로를 결정해야 합니다. 

또한 temporary 파일을 제대로 처리하려면 파일을 생성한 후에 해당 파일을 삭제해주어야 합니다. 이렇게 삭제되지 않은 파일은 우리의 시스템의 디스크 공간을 차지하고 있을 수 있기 때문입니다. 따라서 우리는 temporary 파일이 생성된 후 작업이 완료되고 필요가 없어진 시점에 해당 파일을 자동으로 삭제해주는 함수를 사용하는 것이 좋습니다.

## 더 알아보기

아래 링크들을 참고해 TypeScript에서 temporary 파일을 만드는 더 많은 방법에 대해 알아볼 수 있습니다.

- https://typescript.kr/docs/handbook/advanced-types.html#tempFiles
- https://react-etc.vlpt.us/01.typescript-intro.html
- https://perfectacle.github.io/2019/11/24/typescript-temporary-file/ 

## 관련 링크

- [fs.writeFile 공식 문서](https://nodejs.org/api/fs.html#fs_filesystem) 
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/home.html)
- [임시 파일 관련 블로그 포스트](https://medium.com/@mhagemann/tempfiles-tmpfiles-and-temporary-files-in-typescript-are-possible-765f15c9d1c2)