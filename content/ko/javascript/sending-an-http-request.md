---
title:                "Javascript: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것이 중요한 이유는 프로그래밍에서 자주 사용되기 때문입니다. 웹 서버와 상호작용하거나 데이터를 가져오기 위해서는 HTTP 요청을 보내는 것이 필수적입니다. 또한 HTTP 요청을 이용하면 원격 서버에 데이터를 보내거나 수정할 수도 있습니다.

## 방법

HTTP 요청을 보내는 방법은 간단합니다. 우선, `XMLHttpRequest` 객체를 생성해야 합니다. 그리고 해당 객체의 `open()` 메소드를 사용해 요청을 보낼 URL과 요청 방식을 지정합니다. 예를 들어, `GET` 방식으로 `https://example.com`으로 요청을 보내려면 다음과 같은 코드를 작성합니다.

```javascript
let request = new XMLHttpRequest();
request.open("GET", "https://example.com");
```

다음으로, `send()` 메소드를 사용해 요청을 보냅니다. 만약 데이터를 함께 보내려면 `send()` 메소드의 매개변수로 데이터를 전달해주면 됩니다. 예를 들어, `username`이라는 변수에 저장된 사용자 이름을 함께 보내려면 다음과 같이 작성합니다.

```javascript
request.send("username=" + username);
```

마지막으로, 서버에서 보낸 응답을 받아서 처리합니다. `onreadystatechange` 이벤트 핸들러를 사용해 응답이 도착하면 처리할 로직을 지정해주면 됩니다.

```javascript
request.onreadystatechange = function() {
    // 응답을 받았을 때 수행할 로직 작성
    if (request.readyState === XMLHttpRequest.DONE) {
        console.log("응답 받음!");
        console.log(request.response); // 서버에서 보낸 응답 데이터 출력
    }
}
```

위 예제 코드를 실행하면 응답 데이터가 콘솔에 출력됩니다.

## 심층 분석

HTTP 요청을 보내는 것은 웹 개발에서 매우 중요한 요소입니다. 최신 자바스크립트 프레임워크에서도 HTTP 요청을 위한 라이브러리가 내장되어 있습니다. 또한 웹 애플리케이션의 효율성을 높이기 위해서는 서버와의 통신을 최소화하는 것이 중요합니다. HTTP 요청을 보내는 데는 여러 가지 방법이 있지만, 핵심은 해당 규약에 맞게 요청을 보내고 응답을 처리하는 것입니다.

## 또 다른 정보

- [XMLHttpRequest 객체를 이용한 HTTP 요청 보내기](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest)
- [Fetch API를 이용한 HTTP 요청 보내기](https://developer.mozilla.org/ko/docs/Web/API/Fetch_API/Using_Fetch)
- [Axios 라이브러리를 이용한 HTTP 요청 보내기](https://github.com/axios/axios)