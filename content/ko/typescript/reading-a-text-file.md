---
title:                "TypeScript: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜 문서 파일을 읽는 것일까?

문서 파일은 우리가 쉽게 읽고 이해할 수 있는 형식으로 정보를 담고 있습니다. 때문에 프로그램에서도 이러한 문서 파일을 읽어와서 필요한 정보를 추출해내는 경우가 있습니다. 예를 들면, 우리가 특정한 데이터를 처리하기 위해 문서 파일로 저장된 정보를 필요로 할 때, 우리는 이러한 파일을 읽어와서 데이터를 추출하고 활용할 수 있습니다. 따라서 프로그래머로서 문서 파일을 읽는 방법을 익히는 것은 매우 중요합니다.

## 어떻게 하면 문서 파일을 읽을 수 있을까?

우리는 TypeScript를 이용하여 문서 파일을 읽는 방법을 살펴볼 것입니다. 먼저, 파일 시스템 모듈을 import하고 `readFile()` 메소드를 사용하여 파일을 읽습니다. 그리고 우리는 `toString()` 메소드를 사용하여 파일의 내용을 문자열로 변환할 수 있습니다. 아래는 예시 코드입니다.

```TypeScript
import fs from 'fs';
fs.readFile('file.txt', (err, data) => {
  if (err) throw err;
  const content = data.toString();
  console.log(content);
});
```

위 코드를 실행하면 `file.txt` 파일의 내용이 콘솔에 출력됩니다. 이처럼 간단하게 파일을 읽어와서 내용을 활용할 수 있습니다.

## 깊이 파고드는 정보

문서 파일을 읽는 것은 단순히 내용을 출력하는 것만이 아닙니다. 우리는 파일의 내용을 분석하여 필요한 정보를 추출하고 활용할 수 있습니다. 예를 들면, CSV 파일에서 데이터를 추출하거나 JSON 파일에서 특정 필드의 값만을 가져오는 등의 작업을 할 수 있습니다. 이를 위해서는 파일 내용을 적절한 형식으로 변환하는 작업과 데이터를 추출하는 것에 대한 처리 로직을 추가해주어야 합니다.

# 또 다른 정보

- [TypeScript 문서](https://www.typescriptlang.org/docs)
- [Node.js 파일 시스템 모듈 문서](https://nodejs.org/api/fs.html)
- [문서 파일을 읽는 예제 코드](https://github.com/tf2magazines/ReadingTextFile)