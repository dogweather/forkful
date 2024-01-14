---
title:    "TypeScript: 텍스트 파일 읽기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 방법은 많지만, 타입스크립트로 이를 처리하는 것은 다른 언어들보다 매우 손쉽고 효율적입니다.

## 방법
먼저, 타입스크립트로 파일을 읽기 위해서는 `fs` 모듈을 `import` 해와야 합니다.

```typescript 
import * as fs from 'fs';
```

다음으로, `readFile()` 메소드를 이용하여 읽을 파일의 경로와 인코딩 타입을 전달합니다.

```typescript
fs.readFile('/path/to/file.txt', 'utf8', (err, data) => {
    if (err) {
        console.log(err); // 에러 메시지 출력
    }
    console.log(data); // 파일 내용 출력
});
```

위의 예제 코드에서 `data`는 읽어들인 파일의 내용을 담고 있습니다. 이를 원하는 대로 가공하고 활용할 수 있습니다.

## 깊게 들어가보기
`readFile()` 이외에도 `readFileSync()`와 같은 동기적인 메소드도 있지만, 일반적으로 비동기적인 방식을 더 자주 사용합니다. 또한 `fs` 모듈 이외에도 `stream` 모듈을 이용하여 더 큰 파일도 효율적으로 처리할 수 있습니다.

## 더 알아보기
[ko.javascript.info/textfiles](https://ko.javascript.info/textfiles)  
[developer.mozilla.org/ko/docs/Web/API/File/Using_files_from_web_applications](https://developer.mozilla.org/ko/docs/Web/API/File/Using_files_from_web_applications)

## 참고
[타입스크립트 핸드북 - fs 모듈](https://typescript-kr.github.io/pages/Node.js%20Project.html#typescript-fs-%EB%AA%A8%EB%93%88)