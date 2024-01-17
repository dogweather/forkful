---
title:                "웹 페이지 다운로드하기"
html_title:           "TypeScript: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지를 다운로드한다는 것은 인터넷에서 특정 웹사이트의 데이터를 내 컴퓨터로 가져오는 것을 의미합니다. 이를 프로그래머들은 다양한 목적으로 사용할 수 있습니다. 예를 들어, 웹 페이지 데이터를 가공하여 다양한 시각화를 만들거나, 웹사이트 크롤링을 통해 데이터를 수집하는 등의 용도로 사용할 수 있습니다.

## 하는 방법:

TypeScript를 사용하여 웹 페이지를 다운로드하는 방법을 살펴보겠습니다. 첫째로, HTTP 요청을 보내어 웹 페이지를 가져와야 합니다. 이를 위해서는 HTTP 라이브러리를 사용하거나, 더 간편한 Fetch API를 사용할 수 있습니다. 둘째로, 웹 페이지의 데이터를 가공하여 필요한 정보를 추출하거나 시각화할 수 있습니다. 아래는 TypeScript 코드 예시입니다.

```TypeScript
// HTTP 라이브러리를 사용하는 방법
import * as http from 'http';

http.get('https://www.example.com', (res) => {
    let rawData = '';

    res.on('data', (chunk) => {
        rawData += chunk;
    });

    res.on('end', () => {
        console.log(rawData);
        // 가져온 데이터를 가공할 수 있습니다. 
    });
});

// Fetch API를 사용하는 방법
fetch('https://www.example.com')
    .then(response => response.text())
    .then(text => console.log(text));
```

## 더 파헤쳐보기:

웹 페이지를 다운로드하는 방법은 간단하지만, 프로그래머들은 다양한 라이브러리와 API를 사용하여 좀 더 효율적으로 데이터를 처리할 수 있습니다. 예를 들어, Puppeteer 라이브러리는 웹 페이지를 렌더링하고 자바스크립트를 실행하여 JavaScript 코드로 웹 페이지를 스크래핑할 수 있습니다. 또한, Cheerio 라이브러리를 사용하여 jQuery 스타일의 선택자를 사용하여 DOM을 처리할 수도 있습니다. 웹 페이지 다운로드에 대해 더 깊이 알아보려면 아래 링크를 참고하세요.

## 더 알아보기:

- [Puppeteer 라이브러리](https://github.com/puppeteer/puppeteer)
- [Cheerio 라이브러리](https://cheerio.js.org/)