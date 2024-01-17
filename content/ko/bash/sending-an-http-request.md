---
title:                "HTTP 요청 보내기"
html_title:           "Bash: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTTP 요청을 보내는 것은 우리가 인터넷에서 정보를 가져오는 데에 매우 중요한 일입니다. 그것은 컴퓨터 프로그램이 서버에게 어떤 정보를 요청하고, 서버가 그에 따른 응답을 주는 과정을 말합니다. 프로그래머들은 HTTP 요청을 보내는 것으로 인터넷에서 정보를 가져오거나 전송하는 기능을 프로그램에 추가할 수 있습니다. HTTP 요청은 웹 브라우저로부터 이메일 클라이언트까지 다양한 인터넷 프로그램에서 사용될 수 있습니다.

## 하는 방법:

HTTP 요청을 보내는 것은 Bash 스크립트에서 매우 간단한 일입니다. 다음 코드 블록 안에 코드를 작성하면 됩니다.

```Bash
curl http://example.com
```

위의 코드는 서버에게 http://example.com 이라는 사이트에 대한 요청을 보냅니다. 이 코드를 실행하면, 서버로부터 해당 사이트의 HTML 코드가 출력될 것입니다.

HTTP 요청을 보내는 또 다른 방법은 wget을 사용하는 것입니다. 다음과 같은 코드로 특정 파일을 다운로드할 수 있습니다.

```Bash
wget http://example.com/sample.pdf
```

## 깊이 보기:

HTTP 요청은 현대 인터넷에서 굉장히 중요한 개념입니다. 1990년대 이후, 웹 페이지를 볼 수 있고 이메일을 주고받을 수 있는 데에 HTTP 요청이 매우 큰 역할을 했습니다. 또한, HTTP 외에도 FTP, SMTP 등 다른 프로토콜을 사용하여 데이터를 주고받는 방법도 있지만, HTTP가 최신 웹애플리케이션에서 가장 보편적이며 인기 있는 방식입니다.

HTTP 요청을 보내는 방법은 Bash 뿐만 아니라 다른 프로그래밍 언어에서도 동일하게 사용됩니다. 이를 통해 프로그래머들은 인터넷과 상호작용하며 다양한 데이터를 가져오거나 전송할 수 있도록 도와줍니다.

## 참고 자료:

- [HTTP 요청과 응답](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)
- [Bash 스크립트란? (위키백과)](https://ko.wikipedia.org/wiki/Bash)