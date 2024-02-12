---
title:                "텍스트 파일 읽기"
aliases:
- ko/typescript/reading-a-text-file.md
date:                  2024-01-20T17:55:11.142764-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 파일 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 읽기는 파일의 내용을 코드로 가져오는 것입니다. 프로그래머들은 데이터를 불러오거나 설정을 읽기 위해 이 작업을 합니다.

## How to (방법)
```TypeScript
import * as fs from 'fs';

// 동기적인 방법
const content = fs.readFileSync('example.txt', 'utf8');
console.log(content);

// 비동기적인 방법
fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```
출력 예시:
```
Hello, world!
This is content from the text file.
```

## Deep Dive (심화학습)
텍스트 파일을 읽는 것은 오래된 작업이며, 다양한 프로그래밍 언어에서 지원됩니다. 예전엔 저수준의 API를 직접 다뤄야 했지만, Node.js와 TypeScript에서는 fs(File System) 모듈을 통해 보다 쉽게 파일을 읽거나 쓸 수 있습니다. 다른 방법으로는 streams를 사용하거나 써드파티 라이브러리를 활용할 수 있습니다. 동기적 방법은 파일을 끝까지 읽고 난 후 다음 코드를 실행하지만, 비동기 방법은 파일 읽기 작업이 끝나기를 기다리지 않고 바로 다음 작업을 실행합니다. 큰 파일을 다룰 때는 비동기 방법이 효과적입니다.

## See Also (참고자료)
- Node.js File System Documentation: https://nodejs.org/api/fs.html
- Understanding readFile vs readFileSync in Node.js: https://nodejs.dev/learn/the-nodejs-fs-module
- Stream API in Node.js: https://nodejs.org/api/stream.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
