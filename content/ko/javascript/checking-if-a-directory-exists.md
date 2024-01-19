---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?

디렉토리가 존재하는지 확인하는 것은 그 디렉토리에 파일을 저장하거나 읽기 전에 디렉토리가 실제로 존재하는지 프로그래머가 확인하는 방법입니다. 이를 통해 실행 오류를 방지하고 데이터 손실을 방지 할 수 있습니다.

## 어떻게 해야 합니까:

아래는 Node.js 환경에서 `fs` 모듈을 이용해 디렉토리 존재 여부를 확인하는 자바스크립트 코드입니다:

```Javascript
const fs = require('fs');

if (fs.existsSync('/path/to/directory')) {
  console.log("디렉토리가 존재합니다!");
} else {
  console.log("디렉토리가 존재하지 않습니다.!");
}
```

## 깊은 이해:

디렉토리가 존재하는지 확인하는 과정은 프로그래밍의 오래된 문제입니다. 이를 위해 여러 방법과 라이브러리가 제공되었지만, Node.js에서는 `fs.existsSync()`를 이용해 이 문제를 간단하게 해결합니다.

반면에, 존재하지 않는 디렉토리에 접근하려고 시도할 경우 오류가 발생하므로, `fs.existsSync()`는 이러한 상황을 피하기 위해 사용됩니다. 

다른 방법으로는 `fs.stat()` 또는 `fs.access()` 메소드를 사용하여 비동기적으로 디렉토리의 존재 유무를 확인하는 것입니다. 그러나 이 경우에는 콜백 함수나 Promise를 통한 에러 처리 방식이 더 복잡할 수 있으므로, 간단한 확인 작업에는 `fs.existsSync()`가 더 적합합니다.

## 참고 자료:

- MDN Web Docs에 자바스크립트와 파일 시스템에 대해 더 깊게 알아보려면, [이 링크](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)를 참조하세요.
- Node.js 공식 문서에서 `fs` 모듈에 대해 더 알아보려면, [이 링크](https://nodejs.org/api/fs.html)를 참조하세요.