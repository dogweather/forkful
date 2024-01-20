---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

제목: Haskell로 웹 페이지 다운로드 하기

### 무엇 및 왜?

웹 페이지를 다운로드 받는 것이란 인터넷에서 특정 웹 문서의 복사본을 로컬 시스템에 저장하는 것을 말합니다. 프로그래머들이 이를 사용하는 주된 이유는 웹 스크래핑, 웹 콘텐츠 분석, 웹 페이지의 백업 등 다양한 목적을 위함입니다.

### 어떻게 할까?

다음의 Haskell 코드는 웹페이지를 다운로드하고 결과를 표시합니다:

```Haskell
import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString as B

main =
  do
    src <- simpleHttp "https://example.com"
    B.putStr src
```
위 코드를 실행하면, "https://example.com" 웹 페이지의 HTML 문서를 출력합니다.

### 깊게 살펴보기

웹페이지 다운로드는 월드 와이드 웹(WWW)의 핵심 부분입니다. 초기 인터넷 사용자들은 FTP와 같은 프로토콜을 사용해 데이터를 가져왔습니다. 1990년대 이후로는 HTTP 프로토콜이 인기를 끌었고, 이는 오늘날 웹 페이지를 다운로드하는 데 사용되는 표준 방식입니다.

다운로드의 대안으로는 API 사용이 있습니다. 웹사이트의 API를 사용하면 웹 페이지의 HTML을 가져오는 대신 구조적인 데이터를 직접 받아올 수 있습니다.

Haskell의 경우, `Network.HTTP.Conduit` 모듈의 `simpleHttp` 함수를 사용하여 웹 페이지를 다운로드합니다. 이 함수는 주어진 URL의 내용을 가져와서 바이트 문자열로 반환합니다.

### 참고 자료

다음 링크는 Haskell로 웹 페이지를 다운로드하는 방법과 관련된 추가 정보를 제공합니다:

1. [Haskell HTTP Conduit Library](https://hackage.haskell.org/package/http-conduit)
3. [Haskell HTTP Client](https://hackage.haskell.org/package/http-client)
4. [Web Programming in Haskell (Video)](https://www.youtube.com/watch?v=2VG3pSi7MMo)

귀하의 Haskell 웹 페이지 다운로드 여행이 유익한 경험이 되기를 바랍니다.