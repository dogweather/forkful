---
title:                "디렉토리가 존재하는지 확인하는 방법"
html_title:           "TypeScript: 디렉토리가 존재하는지 확인하는 방법"
simple_title:         "디렉토리가 존재하는지 확인하는 방법"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?:
디렉토리가 존재하는지 확인하는 것은 프로그래머가 할 일 중 하나입니다. 디렉토리가 있는지 여부를 확인하면 예기치 않은 오류를 방지할 수 있으며, 파일 또는 디렉토리를 생성하기 전에 이미 존재하는 디렉토리인지 확인할 수 있습니다.

## 방법:
TypeScript 내에서 디렉토리가 존재하는지 확인하는 방법은 간단합니다. 아래의 코드를 보세요:

```TypeScript
import { existsSync } from 'fs';

if (existsSync('myDirectory')) {
  console.log('myDirectory exists!');
} else {
  console.log('myDirectory does not exist.');
}
```

위의 코드를 실행하면 결과는 작은 폴더 아이콘이 있는지 여부에 따라 다릅니다. 폴더 아이콘이 있으면 `myDirectory exists!`가, 아니면 `myDirectory does not exist.`가 출력됩니다.

## 깊게 파헤치기:
디렉토리가 존재하는지 확인하는 기능은 파일 시스템의 기본 기능입니다. 파일 시스템은 다른 파일 시스템을 포함하고있을 수 있으므로, 디렉토리의 존재 여부는 매우 중요합니다. 디렉토리가 있는지 여부를 확인하는 다른 방법으로는 `fs.readdirSync()`메서드를 사용하는 방법이 있습니다. 이 메서드는 디렉토리 내부의 파일 목록을 가져오기 때문에, 이 방법을 사용하면 더 많은 정보를 얻을 수 있습니다.

어떤 프로그래밍 언어를 사용하든 이 기능을 지원합니다. 하지만 TypeScript는 예외가 발생할 수 있는지에 대한 확실한 유형을 가지고 있어서 더 안전한 방식으로 디렉토리를 확인할 수 있게 해줍니다.

## 관련 자료:
- [Node.js 공식 문서 - 파일 시스템 모듈](https://nodejs.org/api/fs.html)
- [TypeScript 공식 문서 - 파일 API](https://www.typescriptlang.org/docs/handbook/fine-volumes-1/1-narrowing.html#using-type-predicates)