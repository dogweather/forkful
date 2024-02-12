---
title:                "HTTP 요청 보내기"
aliases: - /ko/typescript/sending-an-http-request.md
date:                  2024-01-20T18:01:03.151348-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTTP 요청을 보내는 것은 웹 서버에 정보를 요청하거나 전송하는 방법입니다. 프로그래머들은 데이터를 가져오거나 원격 서비스와 상호 작용하기 위해 이를 사용합니다.

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
