---
date: 2024-01-20 18:01:03.151348-07:00
description: "How to: (\uBC29\uBC95) `fetch` API\uC640 `axios` \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD55C \uC608\uC81C\uC785\uB2C8\uB2E4. \uBA3C\uC800 \uAE30\
  \uBCF8\uC801\uC778 `fetch` \uC0AC\uC6A9\uBC95\uC744 \uBCF4\uC5EC \uB4DC\uB9AC\uACA0\
  \uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.848544-06:00'
model: gpt-4-1106-preview
summary: "`fetch` API\uC640 `axios` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\
  \uD55C \uC608\uC81C\uC785\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (방법)
`fetch` API와 `axios` 라이브러리를 사용한 예제입니다. 먼저 기본적인 `fetch` 사용법을 보여 드리겠습니다.

```TypeScript
// fetch 사용하기
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('요청 중 오류가 발생했습니다:', error);
  }
}

fetchData('https://api.example.com/data');
```

`axios`를 사용하면 다음과 같습니다:

```TypeScript
// axios 설치 필요: npm install axios
import axios from 'axios';

async function fetchData(url: string): Promise<void> {
  try {
    const response = await axios.get(url);
    console.log(response.data);
  } catch (error) {
    console.error('요청 중 오류가 발생했습니다:', error);
  }
}

fetchData('https://api.example.com/data');
```

## Deep Dive (심층 분석)
HTTP 요청을 보내는 것은 웹의 기본입니다. `XMLHttpRequest`는 과거엔 많이 사용되었지만, 복잡하고 사용하기 불편했습니다. `fetch`는 더 간결하고 modern한 대안이며, 프로미스를 반환합니다. `axios`는 `fetch`보다 기능이 다양하며 인터셉터, 요청 취소 등 추가 기능을 제공합니다.

## See Also (더 보기)
- MDN Web Docs의 `fetch`: [MDN fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- `axios` GitHub 리포지토리: [axios GitHub](https://github.com/axios/axios)
- TypeScript 핸드북: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
