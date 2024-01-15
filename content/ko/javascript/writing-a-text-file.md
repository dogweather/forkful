---
title:                "텍스트 파일 쓰기"
html_title:           "Javascript: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 다양합니다. 가장 일반적인 이유는 사용자나 시스템이 읽고 사용할 수 있는 형식으로 데이터를 저장하기 위해서입니다. 또한 텍스트 파일은 컴퓨터에서 다양한 작업을 할 때 필요한 중요한 구성 요소입니다. 따라서 자바스크립트에서는 텍스트 파일을 작성하는 방법을 알고 있어야 합니다.

## 어떻게

텍스트 파일을 작성하는 방법은 간단합니다. 자바스크립트에서는 `fs` 모듈의 `writeFile` 메소드를 사용하여 텍스트 파일을 작성할 수 있습니다. 아래는 파일에 `Hello World`를 쓰는 예제 코드입니다.

```Javascript
const fs = require('fs');

fs.writeFile('hello.txt', 'Hello World', err => {
  if (err) throw err;
  console.log('파일이 성공적으로 작성되었습니다!');
});
```

위 예제에서 `fs.writeFile` 메소드는 첫 번째 인자로 파일의 경로, 두 번째 인자로 쓰고 싶은 내용을 받습니다. 또한 콜백 함수를 인자로 전달하여 파일 작성이 성공했는지 여부를 확인할 수 있습니다. 위 코드를 실행하면 `hello.txt` 파일이 생성되고 그 안에 `Hello World`라는 내용이 작성됩니다.

## 딥 다이브

텍스트 파일을 작성하는 과정에서 몇 가지 중요한 개념을 알아보겠습니다. 첫 번째는 파일의 경로입니다. 파일은 존재하는 경로에 저장되기 때문에 올바른 경로를 지정해야 합니다. 두 번째는 파일의 인코딩 방식입니다. 기본적으로 `fs.writeFile` 메소드는 `utf-8` 인코딩 방식을 사용합니다. 만약 다른 인코딩 방식을 사용하고 싶다면 세 번째 인자로 전달해야 합니다. 마지막으로 파일 작성이 비동기 방식으로 처리되기 때문에 콜백 함수를 통해 파일 작성이 성공적으로 끝났는지 알 수 있습니다.

## 더 알아보기

- [자바스크립트로 파일 쓰기](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Node.js의 파일 시스템 모듈](https://nodejs.org/api/fs.html)
- [파일 인코딩 방식에 대해 알아보기](https://developer.mozilla.org/en-US/docs/Web/API/Encoding_API)