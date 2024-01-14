---
title:                "Haskell: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜?

웹 페이지를 다운로드하는 것은 매우 중요한 전술입니다. 웹 페이지에는 수많은 데이터와 정보가 포함되어 있으며, 이를 다운로드하여 자신에게 유용한 정보를 추출할 수 있습니다. 따라서 해스켈 프로그래밍 언어를 사용하여 웹 페이지를 다운로드하는 방법에 대해 알아보겠습니다.

## 어떻게?

웹 페이지를 다운로드하기 위해서는 먼저 사용할 패키지를 설치해야 합니다. 패키지 관리자인 Cabal을 사용하여 다음과 같이 패키지를 설치할 수 있습니다.

```Haskell
cabal install http-conduit
```

설치가 완료되면 다음과 같이 코드를 작성하여 웹 페이지를 다운로드할 수 있습니다.

```Haskell
import Network.HTTP.Conduit

main = do
  request <- parseRequest "https://example.com" -- 다운로드할 웹 페이지의 URL 입력
  response <- httpLbs request -- 다운로드한 데이터를 바이트 단위로 저장
  putStrLn $ responseBody response -- 다운로드한 데이터 출력
```

위 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
<!doctype html>
<html>
<head>
  <title>Example Domain</title>

  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style type="text/css">
...
```

## 심층 분석

위 코드에서 사용된 `http-conduit` 패키지는 HTTP 요청을 생성하고, 관리하며, 응답을 처리하는 데 사용됩니다. 또한 `parseRequest` 함수를 사용하여 URL을 파싱하고, `responseBody` 함수를 사용하여 다운로드한 데이터를 문자열로 변환하여 출력합니다.

더 많은 정보를 얻고 싶다면 `http-conduit` 패키지의 공식 문서를 참조해보세요. 또한 `Network.HTTP.Simple` 모듈을 사용하여 더 간단하게 웹 페이지를 다운로드할 수도 있습니다.

## 관련 자료

- [Haskell 공식 문서 - HTTP 패키지](https://hackage.haskell.org/package/http)
- [Haskell 공식 문서 - http-conduit 패키지](https://hackage.haskell.org/package/http-conduit)
- [Haskell 공식 문서 - Network.HTTP.Simple 모듈](https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html)