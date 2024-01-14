---
title:                "Fish Shell: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 왜
인터넷을 사용하면서 우리는 종종 웹사이트나 앱에서 정보를 받아올 때 인증을 받아야 합니다. 이때 인증을 받기 위해서는 HTTP 요청과 함께 인증 정보를 보내야 합니다. 여기서는 Fish 쉘을 이용해서 HTTP 요청에 기본 인증을 포함하는 방법에 대해 알아보겠습니다.

## 어떻게
Fish 쉘에서 HTTP 요청을 보낼 때 기본 인증 정보를 포함하는 방법은 간단합니다. 먼저 `curl` 명령어를 사용하여 요청을 보내도록 하겠습니다.

```Fish Shell
curl -u username:password http://www.example.com
```

위 명령어에서 `-u` 옵션을 사용하여 인증 정보를 전달해줍니다. `username`과 `password`에는 실제 사용할 계정 정보를 입력하시면 됩니다. 이제 요청을 보내면 인증 정보가 함께 전송되어 서버에서 인증을 받게 됩니다.

## Deep Dive
HTTP 요청에 대한 기본 인증은 매우 간단하지만, 보안 측면에서 취약할 수 있습니다. 따라서 더 많은 보안을 위해서는 SSL 인증서를 사용하는 것이 좋습니다.

또한, Fish 쉘에서는 `bass` 명령어를 사용하여 curl을 좀 더 편리하게 사용할 수 있습니다. 예를 들어, 다음과 같이 기본 인증 정보를 변수에 담아서 사용할 수 있습니다.

```Fish Shell
set -g -x USERNAME username
set -g -x PASSWORD password
bass curl -u $USERNAME:$PASSWORD http://www.example.com
```

이렇게 하면 변수에 저장된 인증 정보를 사용하여 curl 명령어를 실행할 수 있습니다.

## See Also
- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [curl 공식 문서](https://curl.haxx.se/docs/)
- [HTTP 요청과 응답에 대한 자세한 정보](https://developer.mozilla.org/ko/docs/Web/HTTP/Overview)