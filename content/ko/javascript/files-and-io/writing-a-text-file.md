---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:27.516728-07:00
description: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uD14D\uC2A4\uD2B8\
  \ \uD30C\uC77C\uC744 \uC791\uC131\uD558\uB294 \uAC83\uC740 \uB85C\uAE45, \uC0AC\uC6A9\
  \uC790 \uC785\uB825 \uB0B4\uBCF4\uB0B4\uAE30, \uD639\uC740 \uC124\uC815 \uBAA9\uC801\
  \uC73C\uB85C \uB370\uC774\uD130\uB97C \uAC04\uB2E8\uD558\uACE0 \uC77D\uAE30 \uC26C\
  \uC6B4 \uD615\uC2DD\uC73C\uB85C \uC0DD\uC131\uD558\uACE0 \uC800\uC7A5\uD558\uB294\
  \ \uAC83\uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uAE30\uB2A5\uC740\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uD504\uB85C\uC138\uC2A4\uC758 \uC218\uBA85\
  \uC744 \uB118\uC5B4\uC11C \uB370\uC774\uD130\uB97C \uC9C0\uC18D\uC801\uC73C\uB85C\
  \ \uC720\uC9C0\uD560 \uD544\uC694\uAC00 \uC788\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158\uC5D0\u2026"
lastmod: 2024-02-19 22:05:14.732345
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\
  \uC77C\uC744 \uC791\uC131\uD558\uB294 \uAC83\uC740 \uB85C\uAE45, \uC0AC\uC6A9\uC790\
  \ \uC785\uB825 \uB0B4\uBCF4\uB0B4\uAE30, \uD639\uC740 \uC124\uC815 \uBAA9\uC801\uC73C\
  \uB85C \uB370\uC774\uD130\uB97C \uAC04\uB2E8\uD558\uACE0 \uC77D\uAE30 \uC26C\uC6B4\
  \ \uD615\uC2DD\uC73C\uB85C \uC0DD\uC131\uD558\uACE0 \uC800\uC7A5\uD558\uB294 \uAC83\
  \uACFC \uAD00\uB828\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uAE30\uB2A5\uC740 \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158 \uD504\uB85C\uC138\uC2A4\uC758 \uC218\uBA85\uC744\
  \ \uB118\uC5B4\uC11C \uB370\uC774\uD130\uB97C \uC9C0\uC18D\uC801\uC73C\uB85C \uC720\
  \uC9C0\uD560 \uD544\uC694\uAC00 \uC788\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \uC5D0\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜?
자바스크립트에서 텍스트 파일을 작성하는 것은 로깅, 사용자 입력 내보내기, 혹은 설정 목적으로 데이터를 간단하고 읽기 쉬운 형식으로 생성하고 저장하는 것과 관련이 있습니다. 이 기능은 애플리케이션 프로세스의 수명을 넘어서 데이터를 지속적으로 유지할 필요가 있는 애플리케이션에 있어 중요합니다. 이를 통해 정보를 저장하고 나중에 검색하거나 공유할 수 있는 방법을 제공합니다.

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
