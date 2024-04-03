---
date: 2024-01-20 17:59:59.693099-07:00
description: "How to: (\uBC29\uBC95) JavaScript\uC5D0\uC11C HTTP \uC694\uCCAD\uC744\
  \ \uBCF4\uB0B4\uB294 \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\uBC95 \uC911 \uD558\uB098\
  \uB294 `fetch` API\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC544\
  \uB798\uB294 `fetch`\uB97C \uC774\uC6A9\uD574 \uAC04\uB2E8\uD55C `GET` \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB294 \uC608\uC81C \uCF54\uB4DC\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.786412-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC00\
  \uC7A5 \uAC04\uB2E8\uD55C \uBC29\uBC95 \uC911 \uD558\uB098\uB294 `fetch` API\uB97C\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (방법)
JavaScript에서 HTTP 요청을 보내는 가장 간단한 방법 중 하나는 `fetch` API를 사용하는 것입니다. 아래는 `fetch`를 이용해 간단한 `GET` 요청을 보내는 예제 코드입니다.

```Javascript
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

만약에 이 코드를 실행하면, 콘솔에 서버로부터 받은 데이터가 출력됩니다:

```Javascript
{ "userId": 1, "id": 1, "title": "Sample Data", "completed": false }
```

POST 요청을 보낼 때는 조금 더 정보를 제공해야 합니다. 예를 들면:

```Javascript
fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    title: 'Foo',
    body: 'Bar',
    userId: 1
  })
})
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

이 코드는 서버에 새로운 데이터를 생성할 것을 요청하며, 성공할 경우 콘솔에 결과를 표시합니다.

## Deep Dive (심층 분석)
HTTP 요청은 웹의 기본 요소입니다. 처음엔 HTML 폼이 주된 방법이었지만, XMLHttpRequest(XHR)가 등장하며 개발자가 더 세밀하게 컨트롤할 수 있게 되었습니다. `fetch` API는 XHR보다 더 간결하고 현대적인 대안이며, Promise 기반으로 더 조직적인 비동기 코드를 작성할 수 있게 해줍니다.

비록 `fetch`가 주류가 되었지만, 서드파티 라이브러리로는 Axios, jQuery의 $.ajax 등이 있어 특정 상황에 더 적합할 수 있습니다.

`fetch` 구현 시 주의할 점은 기본적으로 요청이 실패해도 예외를 던지지 않는 다는 것입니다. 따라서, 정상적으로 응답을 처리하기 위해선 응답의 `ok` 속성을 체크해야 합니다.

```Javascript
fetch('https://api.example.com/data')
  .then(response => {
    if (!response.ok) {
      throw new Error('Network response was not ok ' + response.statusText);
    }
    return response.json();
  })
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

## See Also (참고 자료)
- MDN Web Docs의 Fetch API 사용법: https://developer.mozilla.org/ko/docs/Web/API/Fetch_API/Using_Fetch
- Javascript.info의 Fetch 튜토리얼: https://javascript.info/fetch
- Axios GitHub 페이지: https://github.com/axios/axios
- jQuery API 문서: https://api.jquery.com/jquery.ajax/
