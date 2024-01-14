---
title:                "Javascript: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것은 프로그래머가 자주 반복적으로 하게되는 작업 중 하나입니다. 텍스트 파일은 설정 파일, 데이터베이스 백업 파일 및 다양한 기술 문서에 사용될 수 있으며, 간단하면서도 매우 유용한 방법으로 정보를 저장합니다.

## 작성하는 방법

텍스트 파일을 작성하는 가장 간단한 방법은 내장된 Node.js 메서드인 `fs.writeFileSync()`를 사용하는 것입니다. 이 방법을 사용하면 Javascript 코드 내에서 파일을 만들고 내용을 작성할 수 있으며, 이를 통해 다양한 정보를 저장하고 관리할 수 있습니다.

예시를 보겠습니다:

```Javascript
const fs = require('fs');

// 파일 생성
fs.writeFileSync('example.txt', '안녕하세요! 이것은 텍스트 파일입니다.');

// 파일 읽기
let contents = fs.readFileSync('example.txt', 'utf-8');

console.log(contents);
// 출력: '안녕하세요! 이것은 텍스트 파일입니다.'
```

위의 예시에서는 `fs.writeFileSync()`를 이용하여 'example.txt'라는 파일을 생성하고, 그 내용으로 '안녕하세요! 이것은 텍스트 파일입니다.'을 작성합니다. 그 후에는 `fs.readFileSync()`를 이용하여 해당 파일을 읽고, 그 내용을 `console.log()`를 통해 출력합니다.

## 깊게 들어가보기

텍스트 파일을 작성하는 것은 기술 스택에서 꼭 필요한 기능 중 하나입니다. 이를 통해 데이터를 저장하고 로드하며, 소스 코드를 백업하고, 설정 파일을 관리할 수 있습니다. 뿐만 아니라, 주로 프로그램의 디버깅 및 데이터를 시각화하는 용도로도 사용됩니다.

## See Also (참고)

- [Node.js의 File System 모듈 문서](https://nodejs.org/api/fs.html)
- [Node.js로 텍스트 파일을 읽고 쓰는 방법](https://javatutorial.net/node-js-read-write-file)
- [Node.js를 이용한 데이터 시각화](https://stackabuse.com/data-visualization-with-nodejs-and-canvas/)