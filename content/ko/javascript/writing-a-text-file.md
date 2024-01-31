---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

category:             "Javascript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 쓰기란 문자 데이터를 파일 포맷으로 저장하는 것입니다. 자동화, 데이터 보관, 설정 관리 등을 위해 프로그래머는 이 기능을 사용합니다.

## How to: (방법)
Node.js 환경에서 `fs` 모듈을 이용해 텍스트 파일을 생성하거나 수정할 수 있습니다. 아래는 예제 코드와 출력 결과입니다.

```Javascript
const fs = require('fs');

// 파일에 쓰기
fs.writeFile('example.txt', 'Hello, World!', err => {
  if(err) throw err;
  console.log('파일이 성공적으로 저장되었습니다.');
});

// 파일에 추가하기
fs.appendFile('example.txt', '\n안녕하세요!', err => {
  if(err) throw err;
  console.log('내용이 추가되었습니다.');
});
```
출력:
```
파일이 성공적으로 저장되었습니다.
내용이 추가되었습니다.
```

## Deep Dive (심도있는 탐구)
초기 컴퓨팅 시대부터 데이터 저장은 중요했습니다. 초기에는 펀치 카드, 자기 테이프가 사용됐지만 현재는 파일 시스템과 데이터베이스에 정보를 저장합니다. `fs.writeFile`와 `fs.appendFile`는 Node.js에서 제공하는 기본적인 파일 시스템 쓰기 함수입니다. `writeFile`은 존재하는 파일을 덮어쓰고, `appendFile`은 파일의 끝에 내용을 추가합니다. 웹 브라우저에서는 보안상의 이유로 파일 쓰기 기능에 제한이 있지만, HTML5의 `File API`와 `Blob` 객체를 이용해 클라이언트 사이드에서 일부 작업을 할 수 있습니다.

## See Also (참고 자료)
- Node.js File System Documentation: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN의 File API 안내: [https://developer.mozilla.org/en-US/docs/Web/API/File](https://developer.mozilla.org/en-US/docs/Web/API/File)
- MDN의 Blob 레퍼런스: [https://developer.mozilla.org/en-US/docs/Web/API/Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
