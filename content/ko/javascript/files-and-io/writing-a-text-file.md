---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:27.516728-07:00
description: "\uBC29\uBC95: Node.js \uD658\uACBD\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C\
  \ `fs` (\uD30C\uC77C \uC2DC\uC2A4\uD15C) \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uC774 \uC608\uC81C\uB294 \uD30C\uC77C\uC5D0 \uBE44\uB3D9\uAE30\uC801\uC73C\
  \uB85C \uD14D\uC2A4\uD2B8\uB97C \uC791\uC131\uD558\uB294 \uBC29\uBC95\uC744 \uBCF4\
  \uC5EC\uC90D\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.817709-06:00'
model: gpt-4-0125-preview
summary: "Node.js \uD658\uACBD\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C `fs` (\uD30C\uC77C\
  \ \uC2DC\uC2A4\uD15C) \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC \uD14D\uC2A4\uD2B8\
  \ \uD30C\uC77C\uC744 \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
Node.js 환경에서는 내장된 `fs` (파일 시스템) 모듈을 사용하여 텍스트 파일을 작성할 수 있습니다. 이 예제는 파일에 비동기적으로 텍스트를 작성하는 방법을 보여줍니다:

```javascript
const fs = require('fs');

const data = '안녕, 세상! 이것은 파일에 작성될 텍스트입니다.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('파일이 작성되었습니다.');
});
```

샘플 출력:
```
파일이 작성되었습니다.
```

동기 파일 작성을 위해서는 `writeFileSync`을 사용하세요:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('파일이 작성되었습니다.');
} catch (error) {
  console.error('파일 작성 오류:', error);
}
```

현대 웹 브라우저에서는 파일 시스템 액세스 API가 파일을 읽고 쓸 수 있는 기능을 도입했습니다. 하지만, 사용은 사용자 권한에 따라 달라집니다. 파일을 생성하고 작성하는 방법은 다음과 같습니다:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('안녕, 세상! 이것은 브라우저 텍스트 파일 작성입니다.');
  await writable.close();
}
```

더 복잡한 시나리오나 큰 파일을 다룰 때, 브라우저용 `FileSaver.js`와 같은 서드파티 라이브러리를 선택할 수 있습니다:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["안녕, 세상! 이것은 FileSaver.js에서 온 텍스트입니다."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

기억하세요, 클라이언트 측(브라우저에서) 파일 작성은 보안상의 우려로 인해 제한되며, 사용자의 로컬 디스크에 저장을 요구하는 모든 작업은 보통 그들의 명시적인 허가를 요구합니다.
