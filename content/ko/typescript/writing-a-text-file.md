---
title:                "텍스트 파일 쓰기"
html_title:           "TypeScript: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 이게 뭐야 & 왜 하나요?
텍스트 파일을 쓰는 것이란 무엇인가, 그리고 프로그래머들이 왜 이런 작업을 하는지에 대해 2~3문장으로 설명해보자면, 텍스트 파일은 우리가 컴퓨터에 저장하는 단순한 텍스트 정보를 담고 있는 파일입니다. 프로그래머들은 이러한 텍스트 파일을 사용하여 데이터를 저장하고, 읽고, 처리하는 데에 도움이 되는 도구로 활용합니다.

## 하는 법:
```TypeScript
// 파일 모듈 임포트
import * as fs from 'fs';

// 파일 쓰기
fs.writeFileSync('example.txt', 'This is an example of writing a text file in TypeScript.');

// 파일 읽기
const text = fs.readFileSync('example.txt', 'utf-8');
console.log(text); // Output: This is an example of writing a text file in TypeScript.

// 파일 수정
fs.appendFileSync('example.txt', ' File edited!');
const newText = fs.readFileSync('example.txt', 'utf-8');
console.log(newText); // Output: This is an example of writing a text file in TypeScript. File edited!
```

## 깊이 파헤치기:
텍스트 파일을 쓰는 것은 우리가 컴퓨터에 데이터를 저장하는 가장 기본적인 형태 중 하나입니다. 이는 예전부터 사용해오던 방식으로, 현재도 필요하고 유용한 방식입니다. 그러나 요즘에는 데이터베이스 등 다른 방식으로 데이터를 저장하는 경우가 늘어나고 있습니다. 그래도 텍스트 파일은 여전히 가벼우면서도 특정한 목적에 잘 맞는 데이터 저장 방식으로 자주 사용됩니다.

## 더 알아보기:
- [Node.js의 파일 시스템 모듈](https://nodejs.org/api/fs.html)