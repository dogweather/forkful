---
title:    "Javascript: 디렉터리 존재 여부 확인하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 중요한지 궁금하신가요? 실제 개발 상황에서, 우리는 종종 파일이나 디렉토리가 존재하는지 여부를 확인해야 할 때가 있습니다. 예를 들어, 이미 존재하는 파일에 새로운 데이터를 추가하거나, 존재하지 않는 디렉토리에 새로운 파일을 생성할 때 등등 말이죠. 이러한 상황에서 디렉토리의 존재 여부를 확인하는 것은 매우 중요합니다.

## 하는 방법

디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `fs.existsSync()` 함수를 사용하는 것입니다. 이 함수의 인자로 경로를 전달하면 해당 경로에 디렉토리가 존재하면 true를 반환하고, 그렇지 않으면 false를 반환합니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Javascript
const fs = require('fs');

// 경로가 존재하는지 확인
if (fs.existsSync('myDirectory')) {
    console.log('myDirectory가 존재합니다.');
} else {
    console.log('myDirectory가 존재하지 않습니다.');
}
```

위의 예시 코드에서는 'myDirectory'라는 디렉토리가 현재 존재하는지 확인하고 있습니다. 만약 해당 디렉토리가 존재한다면 콘솔에 'myDirectory가 존재합니다.'라는 메시지가 출력될 것이고, 그렇지 않다면 'myDirectory가 존재하지 않습니다.'라는 메시지가 출력될 것입니다.

## 깊게 파헤치기

이제 디렉토리의 존재 여부를 확인하는 방법을 알았으니, 조금 더 깊게 알아보겠습니다. 앞서 말한대로, `fs.existsSync()` 함수는 인자로 전달된 경로에 디렉토리가 존재하는지 여부를 판단합니다. 그렇다면 이 함수는 어떻게 존재 여부를 판단할까요?

실제로는 이 함수 내부에서 `fs.lstatSync()` 함수를 사용하여 해당 경로의 파일 정보를 얻습니다. 이러한 파일 정보에는 해당 경로가 디렉토리인지 아닌지를 나타내는 정보가 포함되어 있습니다. 따라서 `fs.existsSync()` 함수는 이 정보를 바탕으로 디렉토리의 존재 여부를 판단하는 것입니다.

## 더 알아보기

이번 포스트에서는 디렉토리의 존재 여부를 확인하는 방법에 대해 알아보았습니다. 해당 기능을 활용하여 파일 조작 등 다양한 상황에서 유용하게 사용할 수 있습니다. 더 많은 정보를 알고 싶다면 아래의 링크들을 참조해보세요.

### 참고 링크

- [Node.js 공식 문서 - fs.existsSync() 메소드](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_existssync_path)
- [Node.js 공식 문서 - fs.lstatSync() 메소드](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_lstatsync_path_options)
- [GeeksforGeeks - fs.lstatSync() 메소드](https://www.geeksforgeeks.org/node-js-fs-lstatsync-method/)
- [w3schools - Node.js File System Module](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)

## 더 알아보기