---
title:                "http 요청 보내기"
html_title:           "TypeScript: http 요청 보내기"
simple_title:         "http 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

* 왜 누군가 HTTP 요청을 보내는 것에 참여할까요? 
* 현대 웹 개발에서 HTTP 요청은 필수적인 요소로 자리 잡았습니다. 서버에서 데이터를 가져오거나 웹 애플리케이션과 서버 간의 통신이 필요한 경우, HTTP 요청을 통해 이루어집니다. 따라서 TypeScript 프로그래머라면 HTTP 요청을 보내는 방법을 알 필요가 있습니다.

## 어떻게

```TypeScript
const axios = require('axios'); // axios 라이브러리 가져오기

// GET 요청 보내기
axios.get('https://jsonplaceholder.typicode.com/posts')
    .then(response => {
        console.log(response.data); // 요청 결과 출력
    })
    .catch(error => {
        console.log(error); // 에러 처리
    });

// POST 요청 보내기
axios.post('https://jsonplaceholder.typicode.com/posts', {
    title: 'Example Title',
    body: 'Example Body',
    userId: 1
})
    .then(response => {
        console.log(response.data); // 요청 결과 출력
    })
    .catch(error => {
        console.log(error); // 에러 처리
    });
```

```TypeScript
// 요청 결과 예시 - GET 요청
[
  {
    userId: 1,
    id: 1,
    title: 'example title',
    body: 'example body'
  },
  {
    userId: 1,
    id: 2,
    title: 'example title',
    body: 'example body'
  }
]

// 요청 결과 예시 - POST 요청
{
  userId: 1, 
  id: 101,
  title: 'Example Title',
  body: 'Example Body'
}
```

## 깊게 파고들기

위의 예시에서 사용된 axios 라이브러리는 브라우저와 Node.js 환경에서 모두 사용할 수 있는 HTTP 클라이언트 라이브러리입니다. 이 라이브러리를 통해 다양한 HTTP 요청을 보내고 응답을 받을 수 있습니다. 추가적으로 HTTP 요청 옵션을 설정하거나 요청에 대한 인터셉터를 설정할 수도 있습니다. 또한 TypeScript와의 호환성 때문에 더 쉽게 HTTP 요청을 보낼 수 있습니다.

## 연관 정보

[axios 공식 문서 (영문)](https://github.com/axios/axios)

[axios GitHub 저장소 (영문)](https://github.com/axios/axios)

[npm axios 패키지 (영문)](https://www.npmjs.com/package/axios)