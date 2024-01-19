---
title:                "웹 페이지 다운로드하기"
html_title:           "Arduino: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# TypeScript로 웹 페이지 다운로드하기

## 무엇 & 왜?

웹 페이지를 다운로드한다는 것은, 웹 서버로부터 코드를 가져와 로컬에 저장하는 것입니다. 이를 통해 프로그래머들은 웹사이트의 구조를 분석하거나, 데이터를 수집하고, 자동화된 테스팅을 수행할 수 있습니다.

## 어떻게 하는가:

아래의 코드 블록은 Node.js 환경에서 `Axios` 라이브러리를 사용하여 웹 페이지를 다운로드하는 TypeScript 코드 예시입니다:

```TypeScript
import axios from 'axios';
import fs from 'fs';

async function downloadPage(url: string, outputPath: string) {
    const response = await axios.get(url);
    fs.writeFileSync(outputPath, response.data);
}

downloadPage('https://example.com', 'example.html');
```

위 코드를 실행하면 `example.com` 웹 페이지의 HTML 코드가 `example.html` 파일로 저장됩니다.

## 깊게 알아보기

웹 페이지를 다운로드하는 일은 웹의 초기 시절부터 일반적인 작업이었습니다. 원격 서버에 저장된 HTML 코드를 가져와 로컬에서 분석하거나 실행할 수 있기 때문입니다.

물론, 다른 방법들도 있습니다. `fetch API` 또는 `request`와 같은 라이브러리를 이용하는 것이 대표적인 방법들입니다. 이러한 방법을 선택할 때는, 라이브러리 개발 상태, 요구되는 기능, 그리고 개인의 코딩 스타일에 따라 다르게 선택해 사용할 수 있습니다.

앞서 나온 코드에서는 `Axios` 라이브러리를 사용했습니다. 이 라이브러리의 특징인 Promise 기반 API를 활용하면, 비동기 작업을 처리하는 데 편리하게 사용할 수 있습니다. 또한, 내장된 TypeScript 타입 정의가 있어, TypeScript 환경에서 코드 품질을 높이는 데도 좋습니다.

## 그 밖에 볼 만한 것

- [Axios GitHub](https://github.com/axios/axios)
- [MDN fetch API](https://developer.mozilla.org/ko/docs/Web/API/Fetch_API)
- [request GitHub](https://github.com/request/request)