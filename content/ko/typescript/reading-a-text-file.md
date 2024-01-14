---
title:                "TypeScript: 텍스트 파일 읽기"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그래밍에서 중요한 기술 중 하나입니다. 이를 통해 다양한 형식의 정보를 읽고 처리할 수 있으며, 데이터 분석, 문서 처리 등 여러 가지 목적에 활용할 수 있습니다.

## 방법

아래의 TypeScript 예제와 함께 텍스트 파일을 읽는 방법을 알아보겠습니다. 

```TypeScript
// 파일 시스템 모듈 불러오기
import fs from 'fs';

// 파일 경로 지정
const filePath = 'text.txt';

// readFile 함수를 사용하여 파일 읽기
fs.readFile(filePath, 'utf-8', (err, data) => {
    if (err) {
        // 에러 처리
        console.log('파일을 읽는 중 오류가 발생했습니다.');
    } else {
        // 파일 내용 출력
        console.log(data);
    }
});
```

위의 코드를 실행하면 `text.txt` 파일의 내용이 콘솔에 출력됩니다. `readFile` 함수의 첫 번째 인자로는 읽을 파일의 경로를, 두 번째 인자로는 파일의 인코딩 방식을 전달해야 합니다. 콜백 함수의 첫 번째 인자는 발생한 에러를 받고, 두 번째 인자는 읽어온 파일의 내용을 받습니다.

## 깊이 파고들기

`fs` 모듈을 사용하여 파일을 읽어올 수 있지만, 파일을 읽는 방식에는 여러 가지가 있습니다. 예를 들어, `createReadStream` 함수를 사용하면 파일의 크기가 클 때도 메모리를 효율적으로 관리할 수 있습니다. 또한, `readFile` 함수와 달리 스트림 방식으로 파일을 읽는 경우는 읽기가 완료될 때마다 이벤트를 핸들링할 수 있습니다. 이렇게 유연하고 다양한 방식으로 파일을 읽을 수 있다는 것은 프로그래밍에서 텍스트 파일을 다루는 기술의 중요성을 보여주는 것입니다.

## 더 알아보기

이 글에서는 TypeScript를 사용하여 텍스트 파일을 읽는 방법에 대해 알아보았습니다. 파일을 읽는 것 외에도 파일을 쓰는 방법 등 여러 가지 기술을 익히면 프로그래밍에서 다양한 작업을 더욱 효율적으로 수행할 수 있습니다. 아래의 링크들을 참고하여 더 많은 정보를 얻을 수 있도록 노력해 보세요.

## 참고하기

- [Node.js 공식 문서 - File System](https://nodejs.org/api/fs.html)
- [완전히 다루는 TypeScript 가이드](https://typescript-kr.github.io/) [한국어 번역]
- [MDN Web Docs - 파일 시스템](https://developer.mozilla.org/ko/docs/Web/API/File_System)