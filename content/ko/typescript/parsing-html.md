---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱이란 HTML 문서의 내용을 분석하는 과정을 말합니다. 이는 웹사이트 경우 필요한 정보를 추출하거나 특정 데이터를 변경하거나 조작하기 위해 프로그래머들이 자주 사용합니다.

## 어떻게 하는가:

HTML 파싱을 수행하기 위해 TypeScript에서 사용하는 라이브러리는 'jsdom'입니다. 아래에 간단한 예제를 제시합니다:

```TypeScript
import { JSDOM } from "jsdom";

const dom = new JSDOM(`<body>
  <div id="container">
    <h1>Welcome to my Website</h1>
    <p>This is a sample paragraph</p>
  </div>
</body>`);

const container = dom.window.document.getElementById("container");
console.log(container.innerHTML);
```

위 코드를 실행하면 다음과 같은 결과가 출력됩니다:

```Output
<h1>Welcome to my Website</h1>
<p>This is a sample paragraph</p>
```

## 면밀히 살펴보기:

HTML 파싱이라는 개념은 웹 표준의 발전과 맞물려 확립되었습니다. 초기에는 HTML 파싱 모듈이 별도로 존재하지 않았고, 개발자들이 직접 만든 코드로 HTML을 분석하곤 했습니다. 다른 대안으로는 'cheerio'와 'puppeteer' 등의 라이브러리가 있습니다.
 'jsdom' 라이브러리는 순수 자바스크립트로 DOM을 구현하는 데 목적을 두고 있으며, 결과적으로 Node.js 환경에서 HTML을 파싱하는 데 유용하게 사용되고 있습니다.

## 참고 자료:

HTML 파싱에 대한 추가 정보와 관련 리소스를 확인하실 수 있습니다:

1. JSDOM 라이브러리: https://github.com/jsdom/jsdom
2. Cheerio 라이브러리: https://github.com/cheeriojs/cheerio
3. Puppeteer 라이브러리: https://github.com/puppeteer/puppeteer
4. HTML 파싱에 대한 MDN 설명서: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser