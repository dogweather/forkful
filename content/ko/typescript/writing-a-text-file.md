---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 쓰기는 문자열 데이터를 일반 텍스트 형식으로 디스크에 저장하는 과정입니다. 자동화, 로깅, 설정 관리 등을 위해 프로그래머들이 사용합니다.

## How to: (어떻게 하나요?)
TypeScript에서 파일 시스템 작업을 하려면 Node.js의 'fs' 모듈이 필요합니다. 다음은 간단한 텍스트 파일 작성 예제입니다.

```TypeScript
import * as fs from 'fs';

const data: string = '안녕하세요, TypeScript!';

// 동기적으로 파일 쓰기
fs.writeFileSync('message.txt', data, 'utf-8');
console.log('파일이 성공적으로 저장되었습니다!');

// 비동기적으로 파일 쓰기
fs.writeFile('message.txt', data, 'utf-8', (err) => {
    if (err) throw err;
    console.log('파일이 비동기적으로 저장되었습니다!');
});
```

## Deep Dive (심층 분석)
텍스트 파일 쓰기는 초기 컴퓨팅 시절부터 있었으며, 다양항 저장 매체의 발전과 함께 진화해 왔습니다. 'fs' 외에도 'fs-extra', 'stream', 'async', 'promises' 같은 Node.js 라이브러리를 사용하여 파일을 작성할 수 있습니다. 'fs.writeFile'은 비동기적이며, 'fs.writeFileSync'는 병렬 작업 대신 단일 작업을 위해 동기적입니다.

## See Also (더 보기)
- Node.js File System Documentation: https://nodejs.org/api/fs.html
- Understanding Asynchronous JavaScript: https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
