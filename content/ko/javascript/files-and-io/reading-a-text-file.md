---
date: 2024-01-20 17:54:36.359537-07:00
description: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\uC740 \uD30C\
  \uC77C\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uAC00\uC838\uC640 \uD504\uB85C\uADF8\
  \uB7A8\uC5D0\uC11C \uC0AC\uC6A9\uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uD589\uC704\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815, \uB370\
  \uC774\uD130 \uBD84\uC11D, \uB85C\uADF8 \uCC98\uB9AC \uB4F1\uC744 \uC704\uD574 \uC774\
  \ \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.816164-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\uC740 \uD30C\uC77C\
  \uC5D0\uC11C \uB370\uC774\uD130\uB97C \uAC00\uC838\uC640 \uD504\uB85C\uADF8\uB7A8\
  \uC5D0\uC11C \uC0AC\uC6A9\uD560 \uC218 \uC788\uAC8C \uD558\uB294 \uD589\uC704\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC124\uC815, \uB370\uC774\
  \uD130 \uBD84\uC11D, \uB85C\uADF8 \uCC98\uB9AC \uB4F1\uC744 \uC704\uD574 \uC774\
  \ \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
텍스트 파일을 읽는 것은 파일에서 데이터를 가져와 프로그램에서 사용할 수 있게 하는 행위입니다. 프로그래머들은 설정, 데이터 분석, 로그 처리 등을 위해 이 기능을 사용합니다.

## How to: (방법)
JavaScript에서 파일을 읽으려면 `fs` 모듈이 필요합니다. 아래는 간단한 예제 코드입니다:

```javascript
const fs = require('fs');

fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading file:', err);
    return;
  }
  console.log(data);
});
```

출력 예시:
```
안녕하세요, 파일 내용을 읽었습니다!
```

Node.js에서 `readFile` 대신 `readFileSync`를 사용해 동기적으로 파일을 읽을 수도 있습니다:

```javascript
const fs = require('fs');

try {
  const data = fs.readFileSync('example.txt', 'utf8');
  console.log(data);
} catch (err) {
  console.error('Error reading file:', err);
}
```

## Deep Dive (깊이 알아보기)
Node.js가 처음 등장한 2009년부터 JavaScript는 서버 사이드에서도 널리 사용되기 시작했습니다. `fs` 모듈은 Node.js의 핵심 부분으로, 파일 시스템과 상호 작용하는 데 필수입니다.

텍스트 파일을 읽는 다른 방법으로는 스트림(stream)을 사용하는 것이 있습니다. 스트림을 사용하면 큰 파일도 메모리 효율적으로 처리 가능합니다:

```javascript
const fs = require('fs');
const stream = fs.createReadStream('example.txt', 'utf8');

stream.on('data', function(chunk) {
  console.log('New chunk received:');
  console.log(chunk);
});
```

실행 환경이 브라우저인 경우 `FileReader` API를 사용해야 합니다. 이 경우엔 `fs` 모듈이 있지 않으니 주의하세요.

이러한 파일 읽기 기능들은 Node.js의 비동기 프로그래밍 패러다임을 잘 보여줍니다. 코드가 블로킹(blocking)되지 않아 다른 작업을 동시에 처리할 수 있죠. 이는 시스템의 성능과 활용도를 높여줍니다.

## See Also (더 보기)
- Node.js fs 모듈 공식 문서: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- FileReader API MDN 문서: [https://developer.mozilla.org/en-US/docs/Web/API/FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Node.js 스트림에 대한 가이드: [https://nodejs.org/api/stream.html](https://nodejs.org/api/stream.html)
