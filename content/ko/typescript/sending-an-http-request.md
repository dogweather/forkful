---
title:                "HTTP 요청 보내기"
html_title:           "TypeScript: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것이 무엇인지 정확히 알아야 이해할 수 있습니다. HTTP 요청은 서버에 데이터를 요청하거나 보내는 것을 말합니다. 프로그래머들은 자신이 작성한 코드에서 서버와 상호작용하기 위해 HTTP 요청을 보냅니다. 이를 통해 필요한 데이터를 받아오거나 업데이트할 수 있습니다.

## 하는 법:

```TypeScript
axios.get('example.com')
  .then(function(response) {
    console.log(response.data);
  })
  .catch(function(error) {
    console.log(error);
  });
```

위 코드를 예로 들면, 우리는 axios 라이브러리의 `get()` 메서드를 사용하여 'example.com' 주소로 GET 요청을 보냈습니다. 이후, 서버로부터 받은 응답은 `response` 객체에 담겨서 `.then()` 함수의 콜백 함수로 전달됩니다. 이를 이용하여 우리는 서버로부터 받은 데이터를 사용할 수 있게 됩니다.

## 심층 분석:

HTTP 요청은 1990년대 초부터 사용되기 시작했습니다. 이는 웹 브라우저를 통해 서버와 통신하기 위한 표준 프로토콜로 등장했습니다. 그 후, 다양한 방식의 HTTP 요청을 보내는 방법들이 나타나게 되었습니다. 일반적인 방식으로는 Ajax, Fetch 등이 있으며, 그 밖에도 다른 라이브러리나 프레임워크를 사용할 수 있습니다. 하지만 TypeScript는 기본 내장 모듈인 `http`를 사용하여 HTTP 요청을 보낼 수 있습니다. 이 때 요청 유형에 따라 코드가 달라지므로, 이를 이해하고 활용하는 것이 중요합니다.

## 관련 자료:

- [axios 라이브러리 공식 문서](https://github.com/axios/axios)
- [TypeScript 내장 모듈 `http` 문서](https://www.typescriptlang.org/docs/handbook/2/http.html)