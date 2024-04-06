---
date: 2024-01-20 17:41:20.427538-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uB9CC\uB4DC\uB098\uC694?) \uC784\uC2DC\
  \ \uD30C\uC77C \uC0DD\uC131\uC758 \uC5ED\uC0AC\uB294 \uCEF4\uD4E8\uD130\uC758 \uCD08\
  \uAE30\uC640 \uD568\uAED8 \uC2DC\uC791\uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uC810\uC9C4\
  \uC801\uC778 \uCC98\uB9AC\uB098 \uBC31\uC5C5\uC744 \uC704\uD574 \uC784\uC2DC \uD30C\
  \uC77C\uC740 \uD544\uC218\uC801\uC774\uC5C8\uC8E0. \uC0AC\uC2E4 UNIX \uC2DC\uC2A4\
  \uD15C\uC5D0\uC11C `/tmp` \uB514\uB809\uD130\uB9AC\uB294 \uC784\uC2DC \uD30C\uC77C\
  \ \uC800\uC7A5\uC744 \uC704\uD55C \uD45C\uC900 \uC7A5\uC18C\uC785\uB2C8\uB2E4. \uB300\
  \uCCB4 \uBC29\uBC95\uC73C\uB85C\uB294 \uC778\uBA54\uBAA8\uB9AC \uB370\uC774\uD130\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.298605-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uB9CC\uB4DC\uB098\uC694?) \uC784\uC2DC \uD30C\uC77C\
  \ \uC0DD\uC131\uC758 \uC5ED\uC0AC\uB294 \uCEF4\uD4E8\uD130\uC758 \uCD08\uAE30\uC640\
  \ \uD568\uAED8 \uC2DC\uC791\uB418\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

## How to: (어떻게 만드나요?)
```TypeScript
import { fileSync } from 'tmp';

// 임시 파일 생성하기
const tempFile = fileSync();
console.log(`임시 파일 경로: ${tempFile.name}`);

// 임시 파일 사용하기

// 임시 파일 삭제하기 - 필요없을 때 
tempFile.removeCallback();
```
실행 결과:
```
임시 파일 경로: /tmp/12345-abcdef.tmp
```

## Deep Dive (심도있게 알아보기)
임시 파일 생성의 역사는 컴퓨터의 초기와 함께 시작되었습니다. 점진적인 처리나 백업을 위해 임시 파일은 필수적이었죠. 사실 UNIX 시스템에서 `/tmp` 디렉터리는 임시 파일 저장을 위한 표준 장소입니다. 대체 방법으로는 인메모리 데이터 저장이 있습니다만, 이는 비용이 많이 들고 파일 시스템에 옮기기 전 주의가 필요합니다.

자바스크립트와 타입스크립트에서, `tmp` 모듈같이 파일과 디렉터리를 쉽게 생성하고 관리하는 도구가 있습니다. 생성된 파일은 프로세스가 끝나는 경우 자동 삭제될 수도 있어서, 프로그래머가 직접 관리하지 않아도 됩니다. 하지만 안전을 위해서는 명시적으로 삭제하는 것이 좋습니다.

## See Also (함께 보면 좋은 것들)
- Node.js `fs` 모듈 문서: https://nodejs.org/api/fs.html
- `tmp` npm 패키지: https://www.npmjs.com/package/tmp
- Linux /tmp 디렉터리 관리: https://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/tmp.html
