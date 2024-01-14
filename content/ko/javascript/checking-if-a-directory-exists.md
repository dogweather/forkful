---
title:                "Javascript: 디렉토리가 존재하는지 확인하기."
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 디렉토리가 존재하는지를 확인해야 할까요?

디렉토리가 존재하는지 확인하는 것은 자바스크립트 프로그램 작성시 중요한 부분입니다. 디렉토리가 존재하지 않는 상황에서 해당 디렉토리를 사용하려고 한다면 프로그램 실행 중 에러가 발생할 수 있습니다. 따라서 디렉토리의 존재 여부를 반드시 확인해야 합니다.

## 방법

자바스크립트에서 디렉토리를 존재하는지 확인하는 방법은 간단합니다. 우선 "fs" 모듈을 사용하여 파일시스템에 접근합니다. 그 다음 "existsSync" 함수를 이용하여 디렉토리의 존재 여부를 확인합니다. 아래는 예시 코드와 결과입니다.

```Javascript
// 파일시스템 모듈을 불러옵니다.
const fs = require('fs');

// 디렉토리 경로를 변수에 저장합니다.
const dirPath = 'myDirectory';

// existsSync 함수를 이용하여 디렉토리의 존재 여부를 확인합니다.
if (fs.existsSync(dirPath)) {
  console.log('디렉토리가 존재합니다.');
} else {
  console.log('디렉토리가 존재하지 않습니다.');
}
```

위의 코드를 실행하면 해당 디렉토리의 존재 여부에 따라 적절한 메시지가 출력됩니다.

## 딥 다이브

디렉토리의 존재 여부를 확인하는 방법에는 "existsSync" 함수 외에도 "access" 함수를 사용하는 방법이 있습니다. "access" 함수는 디렉토리 뿐만 아니라 파일의 존재 여부도 확인할 수 있습니다. 또한 "fs.stat" 함수를 사용하여 디렉토리의 상세 정보를 확인할 수도 있습니다.

## 유용한 링크들

- [Node.js 공식 문서 - 파일 시스템 접근하기](https://nodejs.org/api/fs.html)
- [MDN 웹 문서 - existsSync 함수](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/fs/existsSync)
- [MDN 웹 문서 - access 함수](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/fs/access)