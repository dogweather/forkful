---
title:    "Javascript: 디렉토리가 존재하는지 확인하기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜 해야 할까요? 

디렉토리가 존재하는지 확인하는 것이 왜 중요한지 궁금하셨나요? 예를 들어, 프로그램에서 사용자가 지정한 디렉토리가 실제로 존재하는지 확인하는 경우에 필요할 수 있습니다. 이를 통해 에러가 발생하지 않도록 미리 예방할 수 있으며, 코드의 안정성을 높일 수 있습니다.

## 어떻게 해야 할까요?

### 예제 1: exists()

`exists()` 함수는 `fs` 모듈에서 제공하는 메소드로, 해당 디렉토리의 존재 여부를 불리언 값으로 반환해줍니다. 다음은 `exists()` 함수를 이용해 현재 디렉토리의 존재 여부를 확인하는 예제입니다.

```Javascript
var fs = require('fs');

fs.exists('./', function(exists) {
  if (exists) {
    console.log('이 디렉토리는 존재합니다.')
  } else {
    console.log('이 디렉토리는 존재하지 않습니다.')
  }
});
```
**출력 결과: 이 디렉토리는 존재합니다.**

### 예제 2: existsSync()

`exitsSync()` 함수는 `fs` 모듈에서 제공하는 동기 메소드로, 해당 디렉토리의 존재 여부를 불리언 값으로 반환해줍니다. 다음은 `existsSync()` 함수를 이용해 현재 디렉토리의 존재 여부를 확인하는 예제입니다.

```Javascript
var fs = require('fs');

if (fs.existsSync('./')) {
  console.log('이 디렉토리는 존재합니다.')
} else {
  console.log('이 디렉토리는 존재하지 않습니다.')
}
```
**출력 결과: 이 디렉토리는 존재합니다.**

## 딥 다이브

디렉토리의 존재 여부를 확인하는 것은 파일 시스템에서 중요한 부분입니다. 디렉토리가 존재하지 않을 때, 해당 디렉토리를 생성하는 등의 추가적인 작업을 할 수도 있습니다. 프로그래밍에서 자주 사용되는 명령이기 때문에, `fs` 모듈에서 제공하는 다양한 메소드를 학습하시는 것을 추천합니다.

## 이어서 보기

- [Node.js File System Module](https://nodejs.org/api/fs.html)
- [Node.js Working with Directories](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)