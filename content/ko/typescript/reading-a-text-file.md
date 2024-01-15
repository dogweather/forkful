---
title:                "텍스트 파일 읽기"
html_title:           "TypeScript: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽기 위해 이 글을 읽어야 하는 이유는 뭘까요? 그 이유는 TypeScript를 사용하여 텍스트 파일을 처리하는 방법을 배우고 싶기 때문입니다.

## 하는 법

우선, 프로젝트에 TypeScript를 설치해야 합니다. 그리고 파일을 읽는 데 사용할 fs 모듈도 필요합니다. 다음은 코드 예시입니다:

```TypeScript
import * as fs from 'fs';

// fs 모듈을 사용하여 'example.txt' 파일을 읽습니다.
fs.readFile('example.txt', 'utf-8', (err, data) => {
  if (err) {
    // 파일을 읽는 중 에러가 발생하면 에러를 출력합니다.
    console.log(err);
  } else {
    // 파일을 성공적으로 읽으면 읽은 내용을 출력합니다.
    console.log(data);
  }
});
```

위 예시 코드를 실행하면 `example.txt` 파일의 내용이 출력될 것입니다.

## 깊이 파고들기

`fs` 모듈은 Node.js의 내장 모듈 중 하나로, 파일 시스템에 접근할 수 있는 다양한 메서드를 제공합니다. 파일을 읽을 때 사용하는 `readFile` 메서드 외에도 `fs` 모듈의 다른 메서드를 사용할 수 있습니다. 더 자세한 내용은 [Node.js 공식 문서](https://nodejs.org/api/fs.html)를 참고하시기 바랍니다.

## 관련 문서

- [코딩 시작하기: TypeScript 설치하기](https://typescript-kr.bootcss.com/docs/tutorials/getting-started.html)
- [TypeScript 공식 문서](https://typescript-kr.bootcss.com/docs/home.html)