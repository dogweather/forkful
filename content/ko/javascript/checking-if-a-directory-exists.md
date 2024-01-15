---
title:                "디렉토리가 있는지 확인하기"
html_title:           "Javascript: 디렉토리가 있는지 확인하기"
simple_title:         "디렉토리가 있는지 확인하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜

존재하는 디렉토리를 확인하는 것이 왜 중요한지 궁금하신가요? 디렉토리가 존재하지 않으면 파일을 찾거나 저장하는 등의 작업이 실패할 수 있기 때문입니다.

# 어떻게

자바스크립트에서 디렉토리가 존재하는지 확인하는 방법은 다양합니다. 기본적으로는 `fs` 모듈의 `exists()` 함수를 사용하는 것이 가장 일반적이지만, 다른 방법도 있습니다.

```
const fs = require('fs');

// Method 1: using fs.exists()
fs.exists('./directory', (exists) => {
  if (exists) {
    console.log('Directory exists');
  } else {
    console.log('Directory does not exist');
  }
});

// Method 2: using fs.access()
fs.access('./directory', (err) => {
  if (err) {
    console.log('Directory does not exist');
  } else {
    console.log('Directory exists');
  }
});
```

위의 예시 코드에서는 `fs.exists()`와 `fs.access()` 함수를 사용하여 디렉토리가 존재하는지 여부를 확인하고, `console.log()`를 사용하여 결과를 출력하고 있습니다.

# 딥 다이브

`fs.exists()`와 `fs.access()` 함수는 콜백 함수를 이용해 결과를 반환하지만, 보다 직관적이고 자세한 결과를 원한다면 `fs.stat()` 함수를 사용할 수 있습니다.

```
fs.stat('./directory', (err, stats) => {
  if (err) {
    console.error(err);
  } else {
    if (stats.isDirectory()) {
      console.log('Directory exists');
    } else {
      console.log('Directory does not exist');
    }
  }
});
```

`fs.stat()` 함수는 파일의 상세 정보를 반환하는데, `isDirectory()` 함수를 사용하면 해당 파일이 디렉토리인지 아닌지를 확인할 수 있습니다.

# 관련 자료

- [Node.js 공식 문서의 fs 모듈](https://nodejs.org/dist/latest-v16.x/docs/api/fs.html)
- [How to check if a file exists in Node.js](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-exists-in-node-js-ko)