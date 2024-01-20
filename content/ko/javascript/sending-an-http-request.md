---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청을 보내는 것은 응용 프로그램이 서버와 통신할 때 사용하는 일반적인 방법입니다. 이를 통해 프로그래머는 사용자 입력을 처리하거나 특정 작업을 수행한 뒤에 결과를 가져올 수 있습니다.

## 어떻게:
새로운 HTTP 요청을 만드는 가장 간단한 방법을 보겠습니다. 이는 `fetch()` 함수를 사용합니다.
```JS
fetch('https://api.example.com/data', {
  method: 'GET'
})
.then(response => response.json())
.then(data => console.log(data));
```
이 예제에서 `fetch()` 함수는 주어진 URL로 HTTP GET 요청을 보냅니다. 요청이 성공적으로 완료되면, `then()` 메소드를 사용하여 응답 데이터를 처리합니다.

## 딥 다이브
HTTP 요청은 1990년대 초에 Tim Berners-Lee에 의해 개발된 월드 와이드 웹의 핵심 구성 요소 중 하나입니다. fetch() 함수 외에도, JavaScript에서는 `XMLHttpRequest`나 `axios` 라이브러리 등의 다른 방법도 있습니다.

`fetch()`는 모던 브라우저에서만 지원하며, 이전 버전이나 일부 브라우저에서는 작동하지 않을 수 있습니다. 이런 경우나 더 복잡한 요청을 다루려면 `axios`나 `XMLHttpRequest`를 사용할 수 있습니다.

## 참고 자료
1. MDN Web Docs의 [Fetch API](https://developer.mozilla.org/ko/docs/Web/API/Fetch_API)
2. MDN Web Docs의 [XMLHttpRequest](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest)
3. Axios의 [Github 페이지](https://github.com/axios/axios)