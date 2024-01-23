---
title:                "임시 파일 생성하기"
date:                  2024-01-20T17:41:20.427538-07:00
model:                 gpt-4-1106-preview
simple_title:         "임시 파일 생성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 그리고 왜?)
임시 파일 생성은 데이터를 일시적으로 저장할 때 사용합니다. 프로그래머들은 대용량 처리, 보안 데이터 임시 저장, 또는 애플리케이션 충돌 시 데이터 복구를 위해 임시 파일을 만듭니다.

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
