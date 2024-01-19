---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지 다운로드는 웹 서버로부터 웹 페이지의 내용을 사용자의 컴퓨터로 가져오는 작업을 뜻합니다. 이는 프로그래머가 웹 페이지의 정보를 분석하거나, 사이트의 데이터를 백업하거나, 오프라인에서 내용을 확인하기 위해 필요합니다.

## 실습방법:

다음은 `node-fetch` 패키지를 이용해 웹 페이지의 HTML을 다운로드하는 JavaScript 코드입니다. 

```javascript
const fetch = require('node-fetch');

async function downloadWebPage(url) {
    const response = await fetch(url);
    const text = await response.text();
    console.log(text);
}

downloadWebPage('https://example.com');
```

위 코드를 실행하면 `https://example.com`의 HTML 코드가 콘솔에 출력됩니다.

## 깊게 알아보기:

해당 기술은 인터넷이 상용화되면서 더욱 중요해졌습니다. 그러나 웹 페이지를 다운로드 받을 때에는 여러 가지 사항들을 고려해야 합니다. 예를 들어, 웹 사이트의 트래픽을 과도하게 점유하지 않도록 `request`를 적절하게 관리하는 것, `robots.txt` 정책을 준수하는 것 등입니다. 

다운로드 방식은 여러가지입니다. 위의 예는 `node-fetch`를 이용한 방식이지만, `axios`, `request` 등 다른 라이브러리를 이용할 수도 있습니다. 프로그래머는 사용 목적, 특성에 따라 적절한 도구를 선택하여 사용합니다.

## 참고자료:

- [MDN Web Docs: Fetch API](https://developer.mozilla.org/ko/docs/Web/API/Fetch_API)
- [npm: node-fetch](https://www.npmjs.com/package/node-fetch)
- [npm: axios](https://www.npmjs.com/package/axios)
- [작업 관리: setTimeout과 setInterval](https://ko.javascript.info/settimeout-setinterval)
- [robots.txt 관련 정보](https://developers.google.com/search/docs/advanced/robots/intro)