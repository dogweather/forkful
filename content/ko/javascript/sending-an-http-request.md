---
title:                "HTTP 요청 보내기"
html_title:           "Javascript: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 보내는 것의 목적은 다양하겠지만, 대부분의 경우 외부 서버나 API와 통신을 위해 사용됩니다.

## 하는 방법
```Javascript
// Vanilla Javascript 예시
const request = new XMLHttpRequest();
request.open('GET', 'https://example.com/api/data', true);
request.send();
```

```Javascript
// jQuery 예시
$.get('https://example.com/api/data', function(data, status) {
  console.log(data);
});
```

```Javascript
// fetch API 예시
fetch('https://example.com/api/data')
  .then(response => response.json())
  .then(data => console.log(data))
```

출력: 외부 서버 또는 API로부터 받은 데이터가 콘솔에 출력됩니다.

## 깊이 들여다 보기
HTTP 요청을 보내면, 브라우저는 해당 URL로 이동하고 해당 서버에서 응답을 받습니다. 응답 내용에 따라 다양한 작업을 수행할 수 있습니다. 또한, HTTP 헤더나 요청 본문을 수정하여 원하는 방식으로 통신할 수 있습니다.

## 더 읽어보기
- [XMLHttpRequest 사용법](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest)
- [jQuery의 AJAX](https://api.jquery.com/jQuery.ajax/)
- [fetch API 소개 및 사용법](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)