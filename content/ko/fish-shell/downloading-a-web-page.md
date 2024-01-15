---
title:                "웹 페이지 다운로드"
html_title:           "Fish Shell: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜
웹 페이지를 다운로드하는 이유는? 아마도 친구에게 쉽게 공유하기 위해서일 거에요. 

## 하는 법
우선 `curl`을 사용해 URL을 지정해주고 다운로드할 파일의 이름을 지정해줍니다. 그리고 `wget`을 사용해 파일을 다운로드하면 됩니다. 

```Fish Shell
curl -o 파일이름 URL
wget URL
```

예를 들어, `fish.io`라는 웹사이트의 로고를 다운로드하고 싶다면 다음과 같이 입력할 수 있어요.

```Fish Shell
curl -o logo.png https://fish.io/logo.png
wget https://fish.io/logo.png
```

이제 현재 디렉토리에 `logo.png` 파일이 다운로드된 것을 확인할 수 있습니다.

## 깊이 알아보기
웹 페이지를 다운로드할 때 사용되는 프로그램들은 다양한 기능을 제공합니다. `curl`은 다양한 인증 방식을 지원하고 `wget`은 다운로드 속도를 개선하는 기능을 가지고 있어요. 또한, 웹 페이지를 다운로드할 때는 서버의 부하나 네트워크 상황에 따라 다운로드 속도가 달라질 수 있습니다. 이러한 다양한 요소들을 고려하면서 웹 페이지를 다운로드하는 것이 중요합니다.

## 참고 
[Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
[Using cURL to Download a File](https://linuxize.com/post/wget-command-examples/)
[Using wget to Download a File](https://www.computerhope.com/unix/wget.htm)