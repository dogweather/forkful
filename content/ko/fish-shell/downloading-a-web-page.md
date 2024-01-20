---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지를 다운로드하는 것은 인터넷에서 웹 페이지의 내용을 저장하는 것입니다. 프로그래머는 이 작업을 수행하여 웹 페이지의 데이터를 분석하거나 나중에 오프라인에서 사용하기 위해 웹 페이지를 저장합니다.

## 방법

Fish shell에서 웹 페이지를 다운로드하는 방법은 간단합니다. 'curl' 명령을 사용하면 됩니다. 다음은 간단한 예제입니다.

```Fish Shell
curl -o output.html 'http://example.com'
```

위의 명령어는 `example.com`의 웹 페이지를 다운로드하여 'output.html' 파일에 저장합니다. 코드를 실행하면 'output.html'이라는 이름의 파일이 생성됩니다.

## 심층 분석

웹 페이지를 다운로드하는 것은 웹의 초기부터 있는 기능입니다. 'curl'와 같은 도구를 사용하면 복잡한 코드를 작성하지 않고도 웹 페이지를 다운로드할 수 있습니다. 참고로 'curl'은 1990년대 후반에 개발되었으며, 이후로 가장 널리 사용되는 웹 데이터 다운로드 도구 중 하나가 되었습니다.

웹 페이지 다운로드에 대한 대안으로는 `wget`이라는 도구도 있습니다. 어떤 방식이 더 나은지는 특정 상황과 니즈에 따라 다릅니다. 

Fish shell에서 웹 페이지를 다운로드할 때, 내부적으로는 네트워크 연결을 열고 HTTP 또는 HTTPS 프로토콜을 통해 웹 페이지의 데이터를 요청합니다. 서버가 응답하면, 그 데이터를 지정한 파일에 저장합니다.

## 참고 자료

다음의 링크에서 더 많은 정보를 얻을 수 있습니다.

'curl' 공식 문서: https://curl.haxx.se/docs/

'wget' 공식 문서: https://www.gnu.org/software/wget/

Fish shell 공식 문서: https://fishshell.com/docs/current/index.html

웹 페이지 다운로드에 대한 자세한 이론: https://en.wikipedia.org/wiki/Web_crawling