---
date: 2024-01-20 17:55:11.142764-07:00
description: "How to (\uBC29\uBC95) \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294\
  \ \uAC83\uC740 \uC624\uB798\uB41C \uC791\uC5C5\uC774\uBA70, \uB2E4\uC591\uD55C \uD504\
  \uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uC5D0\uC11C \uC9C0\uC6D0\uB429\uB2C8\uB2E4\
  . \uC608\uC804\uC5D4 \uC800\uC218\uC900\uC758 API\uB97C \uC9C1\uC811 \uB2E4\uB904\
  \uC57C \uD588\uC9C0\uB9CC, Node.js\uC640 TypeScript\uC5D0\uC11C\uB294 fs(File System)\
  \ \uBAA8\uB4C8\uC744 \uD1B5\uD574 \uBCF4\uB2E4 \uC27D\uAC8C \uD30C\uC77C\uC744 \uC77D\
  \uAC70\uB098 \uC4F8 \uC218 \uC788\uC2B5\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.678090-06:00'
model: gpt-4-1106-preview
summary: "\uC608\uC804\uC5D4 \uC800\uC218\uC900\uC758 API\uB97C \uC9C1\uC811 \uB2E4\
  \uB904\uC57C \uD588\uC9C0\uB9CC, Node.js\uC640 TypeScript\uC5D0\uC11C\uB294 fs(File\
  \ System) \uBAA8\uB4C8\uC744 \uD1B5\uD574 \uBCF4\uB2E4 \uC27D\uAC8C \uD30C\uC77C\
  \uC744 \uC77D\uAC70\uB098 \uC4F8 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

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
