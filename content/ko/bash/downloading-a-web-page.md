---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지 다운로드는 웹 서버로부터 웹 페이지 내용을 가져와 저장하는 것입니다. 프로그래머들은 API 테스트, 웹 스크래이핑, 웹 사이트 백업 또는 오프라인 접근을 위해 이를 수행합니다.

## 어떻게:

쉘 스크립트 또는 커맨드 라인에서 `curl`이나 `wget` 명령을 사용하여 웹 페이지를 다운로드할 수 있습니다. 

```Bash
# curl을 사용하여 페이지 다운로드
curl -O http://yourwebsite.com

# wget을 사용하여 페이지 다운로드
wget http://yourwebsite.com
```
위 명령들은 "yourwebsite.com" 웹사이트의 내용을 현재 디렉토리에 저장합니다.

## 딥 다이브:

이전에 웹 페이지 다운로드는 직접 TCP/IP 소켓을 열고 HTTP 요청을 수행하거나 FTP(HTTP의 전신)를 사용했습니다. `curl`과 `wget`는 그러한 작업을 매우 간단하게 만듭니다.

`curl`과 `wget` 둘 다 강력하며, 대부분의 경우 서로 교체 가능하지만 아래 차이점이 있습니다:
- `curl`은 라이브러리 형태로 다른 프로그램에 통합될 수 있습니다.
- `wget`은 재귀 다운로드를 지원하며, 이는 웹사이트 전체 또는 사이트의 특정 섹션을 다운로드하는데 유용합니다.

## 참고 자료:

- cURL 공식 문서 : https://curl.se/docs/
- Wget 공식 문서 : https://www.gnu.org/software/wget/
- 웹 스크래핑에 대한 광범위한 가이드 : https://realpython.com/python-web-scraping-practical-introduction/