---
date: 2024-01-20 17:44:42.242692-07:00
description: "How to: (\uBC29\uBC95) \uACFC\uAC70\uC5D0\uB294 \uC6F9 \uD398\uC774\uC9C0\
  \uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30 \uC704\uD574 FTP\uC640 \uAC19\uC740\
  \ \uD504\uB85C\uD1A0\uCF5C\uC774 \uC0AC\uC6A9\uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uC9C0\
  \uAE08\uC740 HTTPS\uB97C \uD1B5\uD574 \uBCF4\uC548\uC774 \uAC15\uD654\uB41C \uB370\
  \uC774\uD130 \uC804\uC1A1\uC774 \uC774\uB8E8\uC5B4\uC9D1\uB2C8\uB2E4. \uC704 \uC608\
  \uC81C\uB294 Node.js\uC758 'https' \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC6F9\
  \ \uD398\uC774\uC9C0\uC758 \uB370\uC774\uD130\uB97C \uBC84\uD37C\uC5D0 \uB2F4\uC740\
  \ \uD6C4, \uD569\uCCD0\uC11C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.011097-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uACFC\uAC70\uC5D0\uB294 \uC6F9 \uD398\uC774\uC9C0\uB97C\
  \ \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30 \uC704\uD574 FTP\uC640 \uAC19\uC740 \uD504\
  \uB85C\uD1A0\uCF5C\uC774 \uC0AC\uC6A9\uB418\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## How to: (방법)
```Javascript
const https = require('https');
const fs = require('fs');

const downloadPage = (url, filename) => {
  https.get(url, response => {
    const data = [];
    response.on('data', chunk => {
      data.push(chunk);
    });
    response.on('end', () => {
      const completePage = Buffer.concat(data).toString();
      fs.writeFileSync(filename, completePage);
      console.log(`Downloaded and saved to ${filename}`);
    });
  }).on('error', err => {
    console.error('Error fetching page:', err);
  });
};

// 사용 예
downloadPage('https://example.com', 'examplePage.html');
```
Sample Output:
```
Downloaded and saved to examplePage.html
```

## Deep Dive (심층 분석)
과거에는 웹 페이지를 다운로드하기 위해 FTP와 같은 프로토콜이 사용되었습니다. 지금은 HTTPS를 통해 보안이 강화된 데이터 전송이 이루어집니다. 위 예제는 Node.js의 'https' 모듈을 사용하여 웹 페이지의 데이터를 버퍼에 담은 후, 합쳐서 문자열로 변환합니다. 마지막으로 'fs' (파일 시스템) 모듈로 파일에 저장합니다. 대안으로는 `request`나 `axios` 같은 서드 파티 라이브러리가 있습니다. 이러한 라이브러리들은 추가 기능과 사용의 용이함을 제공합니다.

## See Also (추가 정보)
- MDN Web Docs (https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)에서 Fetch API 사용법 배우기
- Axios GitHub repository (https://github.com/axios/axios)에서 Axios 라이브러리 살펴보기
- Node.js File System Documentation (https://nodejs.org/api/fs.html)에서 더 많은 파일 시스템 메소드 확인하기
