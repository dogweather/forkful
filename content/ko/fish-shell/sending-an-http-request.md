---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요한가?
HTTP 요청은 웹 서버에 정보를 요청하거나 전송하는 방법입니다. 개발자들은 이것을 통해 웹 서비스와 상호작용하고, 웹 기반 API에서 데이터를 가져오거나 보내는데 사용합니다.

## 어떻게 사용하는가:
Fish Shell을 사용하여 HTTP 요청을 보내는 방법은 다음과 같습니다. `curl` 명령어를 이용하면 됩니다.

```Fish Shell
echo "curl https://api.github.com/users/octocat/orgs" | fish
```

위와 같이 입력하면, 결과는 아래와 같이 출력됩니다:

```Fish Shell
[
  {
    "login": "GitHub",
    "id": 1
  }
]
```

## 깊이 파보기:
HTTP 요청은 웹의 기본 구성요소로, 1991년에 처음 만들어졌습니다. 이는 웹페이지의 로딩에 필수적이며 이에 의존하는 작업은 무수히 많습니다. 또한 여러 가지 방법으로 HTTP 요청을 보낼 수 있습니다. `curl` 외에 `wget`, `httpie` 등의 도구를 사용하거나, 파이썬의 `requests`, 자바스크립트의 `fetch` 등의 라이브러리를 이용할 수 있습니다. Fish Shell의 경우에는 내부적으로 `curl`을 사용해서 HTTP 요청을 보낼 수 있습니다. 

## 참고자료:
1. [Curl 사용법](https://curl.se/docs/manual.html)
2. [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
3. [HTTP 요청에 대한 자세한 설명](https://developer.mozilla.org/ko/docs/Web/HTTP/Messages)
4. [Github API 문서](https://docs.github.com/en/rest) 

기억하세요, Fish Shell을 통해 HTTP 요청을 보내는 방법은 다양하며 여러 상황에 따라 사용할 도구나 방법을 선택해야 합니다.