---
title:                "HTML 파싱"
date:                  2024-01-20T15:34:14.077934-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

category:             "TypeScript"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTML 파싱은 웹 페이지의 구조와 내용을 분석해서 데이터를 추출하는 과정입니다. 프로그래머들은 일반적으로 자동화된 데이터 수집, 웹 스크레이핑, 내용 검증을 위해 파싱을 합니다.

## How to: (어떻게:)
```TypeScript
// TypeScript에서 HTML 파싱하기 예제

import axios from 'axios';
import { JSDOM } from 'jsdom';

// 웹 페이지를 가져오는 비동기 함수
async function fetchHTML(url: string): Promise<string> {
  const response = await axios.get(url);
  return response.data;
}

// HTML 내용을 파싱하는 함수
function parseHTML(html: string) {
  const dom = new JSDOM(html);
  const document = dom.window.document;

  // 예를 들어 title 태그의 내용을 출력
  const title = document.querySelector('title')?.textContent;
  console.log(title);
}

// 실행
fetchHTML('https://www.example.com').then(html => parseHTML(html));
```
콘솔 출력 예제:
```
'Example Domain'
```

## Deep Dive (심층 분석)
HTML 파싱은 웹의 초창기부터 중요했습니다. 초기에는 정적 HTML 문서에서 데이터를 추출하는 단순한 작업이었지만, 현재는 동적으로 생성되는 콘텐츠를 다루는 복잡성이 있습니다.

대안으로는 정규 표현식(REGEX), HTML 파서 라이브러리 등이 있습니다. 그러나 정규 표현식은 복잡한 HTML에 적합하지 않고, 에러를 유발할 여지가 많습니다. HTML 파서(예: JSDOM, Cheerio 등)는 DOM을 제대로 구성하고 더 안정적으로 데이터를 추출할 수 있게 해줍니다.

TypeScript에서 JSDOM 같은 라이브러리를 사용하면 노드 환경에서 DOM API에 접근할 수 있어서, 브라우저에서만 가능했던 작업들도 서버 측이나 스크립트에서 수행할 수 있습니다.

## See Also (참조)
- [JSDOM GitHub page](https://github.com/jsdom/jsdom)
- [axios GitHub page](https://github.com/axios/axios)
- [Mozilla Developer Network - Parsing and serializing XML](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [Cheerio GitHub page](https://github.com/cheeriojs/cheerio)
