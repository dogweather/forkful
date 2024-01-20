---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
텍스트 파일 읽기란 프로그램이 텍스트 파일의 내용을 읽어오는 것을 의미합니다. 이를 통해 파이썬 스크립트는 사용자의 입력, 설정값, 데이터 등을 파일에서 직접 불러와 사용할 수 있습니다.

## 어떻게 사용하는가?
Javascript를 사용하여 텍스트 파일을 읽는 방법을 알아보겠습니다. 우선 필요한 모듈인 `fs`를 불러옵니다.

```Javascript
const fs = require('fs');
```
그런 다음 `readFile` 함수를 이용하여 텍스트 파일을 읽습니다.

```Javascript
fs.readFile('./example.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```
이 경우 `example.txt`라는 파일의 내용을 콘솔에 출력합니다.

## 깊이 들여다보기
Javascript의 파일 읽기 기능은 Node.js의 등장과 함께 사용 가능해졌습니다. 이전에는 웹 브라우저 기반의 Javascript에서는 로컬 파일 시스템에 접근할 수 없었습니다. `fs` 모듈은 Node.js만의 기능으로, 서버 환경에서 파일을 읽거나 쓸 수 있게 해줍니다.

`readFile` 외에도 `readFileSync`라는 함수를 사용할 수도 있습니다. `readFileSync`는 동기적인 방식으로 파일을 읽습니다. 이는 파일 읽기가 완료되어야만 다음 코드가 실행된다는 의미입니다. 선택사항이며, 상황에 따라 적절한 함수를 고르면 됩니다.

실제로는 파일을 읽는 데 실패했을 때 에러 처리를 좀 더 세밀하게 하는 것이 중요합니다. 파일이 없거나, 접근 권한이 없는 경우 등을 처리해야 할 수 있습니다.

## 참고 링크
- [Node.js fs 모듈 문서](https://nodejs.org/api/fs.html)
- [Javascript 파일 읽기에 관한 MDN 문서](https://developer.mozilla.org/ko/docs/Web/API/FileReader)