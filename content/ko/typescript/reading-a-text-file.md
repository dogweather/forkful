---
title:    "TypeScript: 텍스트 파일 읽기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 데 참여하는 이유를 몇 가지 살펴보겠습니다. TypeScript를 사용하여 이 기술을 습득하면 텍스트 파일을 쉽게 읽고 처리할 수 있습니다.

## 해야 할 일

이 섹션에서는 TypeScript를 사용하여 텍스트 파일을 읽는 방법에 대해 알아보겠습니다. 우선, fs 모듈을 사용하여 텍스트 파일을 읽어오는 법을 살펴보겠습니다.

```
TypeScript
import { readFileSync } from 'fs';

// 파일을 읽어서 버퍼로 저장
const buffer = readFileSync('파일명.txt');

// 버퍼를 문자열로 변환
const data = buffer.toString();

console.log(data); // 파일의 내용이 출력됨
```

위의 코드를 실행하면 파일의 내용이 출력됩니다. 이제 이를 배열로 변환하여 한 줄씩 읽어올 수 있습니다.

```
TypeScript
const lines = data.split('\n'); // 각 줄을 배열로 나눔
lines.forEach((line) => console.log(line)); // 각 줄을 출력
```

또 다른 방법으로는 readline 모듈을 사용하여 한 줄씩 읽어오는 방법이 있습니다.

```
TypeScript
import { createInterface } from 'readline';

const readLine = createInterface({
  input: createReadStream('파일명.txt'),
});

readLine.on('line', (line) => console.log(line)); // 각 줄을 출력
```

## 깊이 들어가기

텍스트 파일을 읽는 것은 단순해 보일 수 있지만, 실제로는 매우 중요한 작업입니다. 텍스트 파일을 읽어와서 데이터를 분석하고 가공하여 유용한 정보를 추출할 수 있습니다. 텍스트 파일을 읽는 것은 많은 언어에서 공통적인 작업이지만, TypeScript의 경우에는 타입 지정을 통해 더 안정적이고 안전하게 데이터를 처리할 수 있습니다.

## 참고

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs)
- [fs 모듈 레퍼런스](https://nodejs.org/docs/latest-v12.x/api/fs.html)
- [readline 모듈 레퍼런스](https://nodejs.org/api/readline.html)