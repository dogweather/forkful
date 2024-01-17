---
title:                "웹 페이지 다운로드"
html_title:           "Bash: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜 하는 거죠?

웹 페이지를 다운로드한다는 것은 해당 페이지의 코드를 복사하여 로컬 시스템에 저장하는 것을 뜻합니다. 이는 프로그래머들이 입력과 출력을 조작하여 원하는 정보를 얻을 수 있게 해주기 때문에 자주 하는 작업입니다.

## 어떻게 하나요?

이제 여러분이 웹 페이지를 다운로드하는 방법에 대해 알아보겠습니다. 먼저, 다운로드하려는 웹 페이지의 주소(URL)를 알아야 합니다. 그리고 다음의 코드를 터미널에 입력하여 웹 페이지를 다운로드할 수 있습니다.

```Bash
wget [URL]
```

이제 다운로드된 페이지는 현재 디렉토리에 `index.html` 파일로 저장될 것입니다. 만약 원하는 다른 이름으로 저장하고 싶다면, 다음과 같이 입력하면 됩니다.

```Bash
wget [URL] -O [파일명]
```

## 깊이 파고들어보기

웹 페이지를 다운로드하는 방법에 대해 더 자세히 알아보겠습니다. wget이라는 명령어는 1996년에 시작되어 현재까지도 많은 사람들에게 사용되고 있습니다. 또한 wget은 다운로드 뿐만 아니라 HTML 페이지 내에 있는 링크들을 따라다니며 모든 웹 페이지를 다운로드하는 것도 가능합니다. 다른 대안으로는 curl이 있습니다. curl은 wget과 비슷한 목적으로 사용되지만, 더 다양한 프로토콜을 지원하는 특징이 있습니다. wget은 HTTP, HTTPS, FTP에 대한 다운로드만 가능하지만, curl은 FTP, FTPS, HTTP, HTTPS, SCP, SFTP 등의 프로토콜을 지원합니다.

또한, 이 모든 다운로드 과정은 HTTP 통신을 통해 이루어집니다. 웹 페이지를 다운로드하면 HTTP 요청을 보내고, 그에 따라 서버에서는 HTTP 응답을 보내게 됩니다. 그리고 해당 응답을 받은 프로그램은 이를 해석하여 우리가 볼 수 있는 웹 페이지로 변환합니다.

## 관련 링크

- https://www.gnu.org/software/wget/ - wget 공식 웹사이트
- https://curl.haxx.se/ - curl 공식 웹사이트