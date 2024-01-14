---
title:                "Javascript: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

### 왜 다운로드하는가?

웹 페이지를 다운로드하는 이유는 다양합니다. 가장 간단한 이유는 페이지를 오프라인에서 볼 수 있기 때문입니다. 또 다른 이유는 페이지 소스 코드를 분석하거나 웹 크롤링을 위해서입니다. 아마도 가장 관련성이 있는 이유는 웹 페이지에 포함된 정보를 분석하고 추출하기 위함입니다.

### 방법

웹 페이지를 다운로드하는 것은 자바스크립트에서 매우 간단하게 할 수 있습니다. 먼저 XMLHttpRequest 라이브러리를 import 해줍니다. 그 다음 웹 페이지 URL을 입력하고 변수로 저장합니다. 마지막으로 open(), send() 메소드를 사용하여 웹 페이지 내용을 가져옵니다.

```Javascript
const xhr = new XMLHttpRequest();
const url = "https://www.example.com";
xhr.open("GET", url, true);
xhr.send();
```

위의 예제에서는 GET 요청을 보냅니다. 원한다면 POST를 사용할 수도 있습니다. 그리고 true 대신 false를 입력하면 비동기 요청이 아니라 동기 요청으로 처리됩니다. 그리고 마지막으로 변수 xhr에 저장된 정보를 출력하면 웹 페이지의 내용을 확인할 수 있습니다.

### 심층 분석

자바스크립트를 사용하면 다양한 방법으로 웹 페이지를 다운로드할 수 있습니다. GET과 POST 방식 외에도, 라이브러리나 프레임워크를 사용하여 더욱 복잡한 요청을 보낼 수 있습니다. 또한 웹 페이지의 특정 부분만 가져오는 것도 가능합니다. 이를 위해 DOM 파싱 등의 기술을 사용할 수 있습니다. 웹 크롤링과 관련된 분야에서는 자바스크립트 기반의 다양한 라이브러리들이 존재하므로, 웹 페이지 다운로드에 대한 심층적인 연구가 필요합니다.

### 참고 자료

- [XMLHttpRequest MDN 문서](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest)
- [axios 라이브러리](https://github.com/axios/axios)
- [cheerio 라이브러리](https://cheerio.js.org/)
- [puppeteer 라이브러리](https://pptr.dev/) (웹 크롤링에 사용되는 자동화 라이브러리)