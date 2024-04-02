---
date: 2024-01-20 17:43:37.772706-07:00
description: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD55C\uB2E4\
  \uB294 \uAC74 \uD574\uB2F9 \uD398\uC774\uC9C0\uC758 \uB0B4\uC6A9\uC744 \uC778\uD130\
  \uB137\uC5D0\uC11C \uB2F9\uC2E0\uC758 \uCEF4\uD4E8\uD130\uB85C \uAC00\uC838\uC624\
  \uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC790\uB3D9\
  \uD654\uB41C \uB370\uC774\uD130 \uC218\uC9D1, \uCF58\uD150\uCE20 \uBC31\uC5C5, \uB610\
  \uB294 \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uD14C\uC2A4\uD305\uC744 \uC704\
  \uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.480197-06:00'
model: gpt-4-1106-preview
summary: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD55C\uB2E4\uB294\
  \ \uAC74 \uD574\uB2F9 \uD398\uC774\uC9C0\uC758 \uB0B4\uC6A9\uC744 \uC778\uD130\uB137\
  \uC5D0\uC11C \uB2F9\uC2E0\uC758 \uCEF4\uD4E8\uD130\uB85C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC790\uB3D9\uD654\
  \uB41C \uB370\uC774\uD130 \uC218\uC9D1, \uCF58\uD150\uCE20 \uBC31\uC5C5, \uB610\uB294\
  \ \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uD14C\uC2A4\uD305\uC744 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## What & Why? (무엇이며 왜?)
웹 페이지를 다운로드한다는 건 해당 페이지의 내용을 인터넷에서 당신의 컴퓨터로 가져오는 것입니다. 프로그래머는 자동화된 데이터 수집, 콘텐츠 백업, 또는 웹 애플리케이션 테스팅을 위해 이 작업을 수행합니다.

## How to (어떻게):
### curl을 사용하는 방법:
```Bash
curl http://example.com -o example.html
```
`example.html` 파일에 `http://example.com`의 내용이 저장됩니다.

### wget 사용법:
```Bash
wget http://example.com
```
현재 디렉토리에 해당 웹 페이지의 내용이 `index.html`로 저장됩니다.

### 샘플 출력:
```Bash
$ curl http://example.com -o example.html
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1270  100  1270    0     0   6569      0 --:--:-- --:--:-- --:--:--  6597
```

## Deep Dive (심층 탐구):
초창기 웹에서는 페이지를 다운로드하기 위해 웹 브라우저를 사용하거나 FTP와 같은 프로토콜을 사용하는 것이 일반적이었습니다. 그러나 스크립트를 사용하여 웹 자원을 자동으로 다운로드하는 방법이 필요하게 되었고, 이를 위해 `curl`과 `wget` 같은 명령줄 툴들이 개발되었습니다.

`curl`은 다양한 프로토콜을 지원하며, 데이터 전송용 라이브러리로도 활용할 수 있습니다. 반면에, `wget`은 재귀적 다운로드 등의 기능을 지원하여 웹사이트의 전체를 손쉽게 미러링할 수 있게 해줍니다.

실제 구현 시, 네트워크 상태나 웹서버 설정에 따라 다운로드가 실패할 수 있습니다. 이를 위해 `curl`과 `wget` 명령어에는 시도 횟수를 지정하거나 다운로드 속도를 제한하는 옵션을 제공합니다.

## See Also (참고자료):
- `curl` 매뉴얼 페이지: [https://curl.se/docs/manpage.html](https://curl.se/docs/manpage.html)
- `wget` 매뉴얼 페이지: [https://www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)
- Bash 스크립팅 안내서: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
