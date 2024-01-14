---
title:                "Bash: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜 HTTP 요청을 보내는가?

HTTP 요청은 컴퓨터의 네트워크 통신에서 중요한 역할을 합니다. 많은 웹 응용 프로그램은 사용자의 요청에 따라 HTTP 요청을 보내고, 이를 기반으로 서버는 요청에 대한 응답을 보내줍니다. 그리고 이를 통해 인터넷을 통해 정보를 주고받을 수 있습니다.

## 어떻게 HTTP 요청을 보낼 수 있는가?

HTTP 요청을 보내는 것은 터미널에서 간단하게 할 수 있습니다. `curl`이나 `wget`과 같은 프로그램을 이용하면 매우 간단하게 요청을 보낼 수 있습니다. 예를 들어, 다음과 같은 커맨드를 입력하면 해당 사이트에서 내용을 불러올 수 있습니다.

```bash
curl https://www.example.com
```

위 커맨드를 실행하면 해당 웹 사이트의 자원들이 터미널에 표시됩니다. 이렇게 받은 정보를 이용해 다양한 목적으로 활용할 수 있습니다.

또한 HTTP 요청을 보내기 위해 직접 소켓을 열어서 요청을 보내는 방법도 있습니다. 이 방법은 더욱 깊이 있는 방법이기 때문에, 아래 "더 알아보기" 섹션에서 좀 더 자세한 정보를 제공하겠습니다.

## 더 알아보기

HTTP 요청을 보내는 것은 컴퓨터 통신에서 중요한 개념이기 때문에, 이것에 대해 좀 더 자세히 알아보는 것이 좋습니다. 이를 위해 다음 링크들을 참고해보세요:

- [HTTP 요청에 대한 더 자세한 정보](https://ko.wikipedia.org/wiki/%EC%88%98%EB%A3%8C%ED%88%AC%EC%9E%90_%ED%86%B5%EC%8B%A0_%EB%B0%A9%EC%8B%9D)
- [curl과 wget 프로그램 사용 방법](https://www.lesstif.com/pages/viewpage.action?pageId=14745786)
- [HTTP 요청의 구조와 방식](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)
- [소켓을 이용한 HTTP 요청 보내기](https://stackoverflow.com/questions/13732826/making-http-requests-using-sockets-only)

## 관련 링크

- [HTTP 프로토콜의 작동 방식](https://asfirstalways.tistory.com/67)
- [HTTP 기본 개념 이해하기](https://novemberde.github.io/%EA%B0%9C%EB%B0%9C/2017/07/18/%EB%AA%A8%EB%84%90%EB%93%A4%EC%9D%80-%EB%A7%8C%EB%93%A4%EA%B0%80%EB%A9%B4-HTTP-%EC%9E%90%EB%A3%8C%EC%B2%B4-1.html)
- [HTTP 요청에 대한 자세한 설명과 예제 코드](https://www.tutorialspoint.com/restful/restful_quick_guide)