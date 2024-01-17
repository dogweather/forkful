---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Javascript: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
디렉토리의 존재 여부를 확인하는 것은 우리가 무엇을 할 수 있는지를 알려줍니다. 복잡한 프로그램에서 특정한 디렉토리가 존재하는지 여부를 확인하고, 적절한 조치를 취하고자 할 때 매우 유용합니다.

## 하는 법:
```Javascript
// 디렉토리 존재 여부 확인 함수
function checkDirectoryExists(path) {
  try {
    fs.accessSync(path)
    return true
  } catch(error) {
    return false
  }
}

// 디렉토리 존재 여부 확인 예제
console.log(checkDirectoryExists('/Users/Kim/Test')) // true
console.log(checkDirectoryExists('/Users/Park/Test')) // false
```

## 깊이 파헤치기:
디렉토리의 존재 여부를 확인하는 기능은 파일 시스템에서 매우 중요합니다. 예를 들어, 사용자가 특정 파일을 다운로드하기 전에 해당 디렉토리가 존재하는지 확인할 수 있고, 그렇지 않다면 존재하지 않는 디렉토리를 만들어줄 수 있습니다.

또한 `fs.exist()`를 사용하여 디렉토리가 존재하는지 확인할 수도 있지만, 해당함수는 성능이 더 좋지 않으며 더 이해하기 어렵습니다. 따라서, `fs.access()`와 `fs.accessSync()`를 사용하는 것이 더 바람직합니다.

## 또한 볼만한 것:
- [Node.js documentation on fs manipulation](https://nodejs.org/dist/latest-v12.x/docs/api/fs.html)
- [GeeksforGeeks article on checking if a directory exists in Node.js](https://www.geeksforgeeks.org/node-js-fs-access-method/)