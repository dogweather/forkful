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

## 무엇 그리고 왜?

HTTP 요청을 보내는 것은 서버로부터 정보를 가져오거나 업데이트하기 위해 인터넷에서 가장 많이 사용되는 방법 중 하나입니다. 프로그래머는 이 기술을 사용하여 웹 애플리케이션을 더욱 동적이고 상호 작용식으로 만들 수 있습니다.

## 하는 법:

```Javascript
// 예시 1: 숫자 더하기
let num1 = 5;
let num2 = 10;

let result = num1 + num2;
console.log(result); // 15 출력
```

```Javascript
// 예시 2: HTTP 요청 보내기
const http = new XMLHttpRequest();
http.open("GET", "https://example.com/api/users");
http.send() // 요청 보내기
http.onload = () => {
  let users = JSON.parse(http.responseText);
  console.log(users); // 서버로부터 받아온 사용자 정보 출력
};
```

## 깊이 들어가보기:

HTTP 요청은 1991년 팀 버너스 리가 대표적인 웹 프로토콜인 HTTP를 발명하면서 처음 소개되었습니다. 그 후 브라우저와 서버 간의 통신을 가능하게 하는 XMLHttpRequest가 등장했고, 이제는 fetch API가 나옴으로써 더욱 간단하게 HTTP 요청을 보낼 수 있게 되었습니다. 또한 HTTP는 웹 외에도 브라우저와 다른 서버 사이에서 데이터를 주고받는 데에도 사용됩니다.

## 참고 자료:

- [MDN Web Docs: HTTP](https://developer.mozilla.org/ko/docs/Web/HTTP)
- [MDN Web Docs: XMLHttpRequest](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest)
- [MDN Web Docs: fetch API](https://developer.mozilla.org/ko/docs/Web/API/Fetch_API)