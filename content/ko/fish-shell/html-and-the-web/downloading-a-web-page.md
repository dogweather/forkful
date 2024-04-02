---
date: 2024-01-20 17:44:19.688748-07:00
description: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uB294 \uC778\uD130\
  \uB137\uC5D0\uC11C \uD398\uC774\uC9C0\uC758 \uB370\uC774\uD130\uB97C \uB85C\uCEEC\
  \uB85C \uBD88\uB7EC\uC624\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uBD84\uC11D, \uBC31\uC5C5, \uC790\uB3D9\uD654\
  \uB41C \uD14C\uC2A4\uD305 \uD639\uC740 \uCF58\uD150\uCE20 \uC2A4\uD06C\uB798\uD551\
  \uC744 \uC704\uD574 \uC774\uAC83\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.853872-06:00'
model: gpt-4-1106-preview
summary: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uB294 \uC778\uD130\uB137\
  \uC5D0\uC11C \uD398\uC774\uC9C0\uC758 \uB370\uC774\uD130\uB97C \uB85C\uCEEC\uB85C\
  \ \uBD88\uB7EC\uC624\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB370\uC774\uD130 \uBD84\uC11D, \uBC31\uC5C5, \uC790\uB3D9\uD654\uB41C\
  \ \uD14C\uC2A4\uD305 \uD639\uC740 \uCF58\uD150\uCE20 \uC2A4\uD06C\uB798\uD551\uC744\
  \ \uC704\uD574 \uC774\uAC83\uC744 \uD569\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## What & Why? (무엇 그리고 왜?)
웹 페이지 다운로드는 인터넷에서 페이지의 데이터를 로컬로 불러오는 것입니다. 프로그래머들은 데이터 분석, 백업, 자동화된 테스팅 혹은 콘텐츠 스크래핑을 위해 이것을 합니다.

## How to: (어떻게:)
Fish Shell에서는 `curl`이나 `wget` 명령어를 사용하여 웹 페이지를 다운로드할 수 있습니다. 다음은 간단한 예시와 결과물입니다.

```Fish Shell
# curl을 사용하여 웹 페이지 내용을 파일로 저장
curl http://example.com -o example_page.html

# wget을 사용하려면 다음과 같이 입력합니다.
wget http://example.com -O example_page.html
```

이 명령을 실행한 후에는, `example_page.html` 파일 안에 웹 페이지의 내용이 저장됩니다.

## Deep Dive (깊이 있게 알아보기)
과거에는 웹 페이지를 다운로드할 때 FTP 프로토콜을 주로 사용했었습니다. 하지만 보안 문제와 속도 때문에 HTTP와 HTTPS가 더 널리 쓰이게 되었습니다.

`wget`은 고전적인 도구로, 재귀적 다운로드와 오프라인 브라우징을 위한 강력한 기능을 가지고 있습니다. `curl`은 더 다양한 프로토콜을 지원하고 커스터마이징에 유연하여 개발자 사이에서 인기가 높습니다.

실제 구현에서, `curl`은 단일 파일을 가져올 때 적합하며, `wget`은 웹사이트 전체를 미러링하는 데 유용합니다.

## See Also (더 알아보기)
- `curl` 사용법에 대한 공식 문서: https://curl.se/docs/manpage.html
- `wget` 사용법에 대한 공식 문서: https://www.gnu.org/software/wget/manual/wget.html
- 다양한 프로토콜 지원과 관련된 `curl` 정보: https://curl.se/docs/proto.html
- 웹 데이터 스크래핑 기본: https://realpython.com/python-web-scraping-practical-introduction/
