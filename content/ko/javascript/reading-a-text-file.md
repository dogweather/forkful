---
title:    "Javascript: 텍스트 파일 읽기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것이 중요한 이유는 데이터에 대한 접근이 필요하기 때문입니다. 이를테면, 웹사이트에서 사용자 정보를 저장하기 위해 텍스트 파일에 데이터를 저장하거나, 로그인 정보를 읽는 등 다양한 용도로 텍스트 파일을 읽을 수 있습니다.

## 방법

아래의 코드 블록을 통해 간단한 자바스크립트 예제와 출력을 확인할 수 있습니다.

```Javascript
// 파일 시스템 모듈을 호출합니다.
const fs = require('fs');

// 읽을 파일의 경로를 설정합니다.
const filePath = 'file.txt';

// readFile 메소드를 사용해 파일을 읽습니다.
fs.readFile(filePath, 'utf8', (err, data) => {
  // 오류 처리를 합니다.
  if (err) {
    console.log(err);
    return;
  }
  // 읽은 파일의 내용을 출력합니다.
  console.log(data);
});
```

위의 코드는 `file.txt`라는 텍스트 파일을 읽고 해당 파일 내용을 출력하는 예제입니다.

## 더 자세히

텍스트 파일을 읽는 방법은 여러 가지가 존재합니다. 위의 예제에서는 `fs.readFile` 메소드를 사용했지만, `fs.readFileSync`와 같은 다른 메소드를 사용할 수도 있습니다. 또한 파일의 인코딩 방식이나 콜백 함수를 지정하는 방법 등 여러 가지 옵션을 제공합니다. 이러한 옵션들을 적절히 이용하면 보다 정확하고 유연한 파일 읽기가 가능합니다.

## 참고

- [Node.js 공식 문서 - 파일 시스템 모듈](https://nodejs.org/api/fs.html)
- [생활코딩 - Node.js 파일 읽기 강좌](https://opentutorials.org/course/3332)
- [ZeroCho's Devlog - Node.js 파일 읽은 뒤 작업하기](https://www.zerocho.com/category/NodeJS/post/5837d7004297340018ada51c)