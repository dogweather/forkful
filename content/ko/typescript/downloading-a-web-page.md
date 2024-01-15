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

## 왜

웹 페이지를 다운로드하는 일에 참여할 이유는 바로 웹 페이지의 콘텐츠를 오프라인에서도 볼 수 있기 때문입니다.

## 다운로드하는 방법

Downloading a web page can be easily accomplished using TypeScript's built-in `fetch()` function. Simply provide the URL of the web page as an argument and the function will return a promise that resolves with the response from the server.

```TypeScript
fetch('https://www.example.com')
    .then(response => response.text())
    .then(html => console.log(html))
```

The code above will log the HTML content of the web page to the console. For more complex operations, you can also pass in an optional configuration object as the second argument to `fetch()`.

```TypeScript
fetch('https://www.example.com', {
    method: 'POST',
    headers: {
        'Content-Type': 'application/json'
    },
    body: JSON.stringify({ username: 'JohnDoe', password: '12345' })
})
    .then(response => response.json())
    .then(data => console.log(data));
```

In this example, we are using `fetch()` to make a POST request to the server with some JSON data in the body. The response is then parsed as JSON and logged to the console.

## 깊이 파헤치기

Behind the scenes, TypeScript's `fetch()` function uses XMLHttpRequest or the newer Fetch API to handle the HTTP request. It automatically resolves redirects and follows the proper protocol for handling cookies.

Additionally, `fetch()` also allows you to set custom headers and handle different types of content, such as JSON or forms. It also supports authentication methods like OAuth or Basic authentication.

However, keep in mind that `fetch()` may not work in all browsers, so it's always a good idea to have a fallback option in case it is not supported.

## 관련 링크

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)
- [Fetch API 문서](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [XMLHttpRequest 문서](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)