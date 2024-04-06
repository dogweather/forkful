---
date: 2024-01-26 04:34:17.250134-07:00
description: "\uBC29\uBC95: XML, \uB610\uB294 eXtensible Markup Language\uB294 90\uB144\
  \uB300 \uB9D0\uBD80\uD130 \uB4F1\uC7A5\uD574 \uAD6C\uC870\uD654\uB41C \uB370\uC774\
  \uD130\uB97C \uC704\uD55C \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uB418\uB294\
  \ \uD615\uC2DD\uC73C\uB85C \uB0A8\uC544 \uC788\uC2B5\uB2C8\uB2E4. PowerShell\uC740\
  \ \uC804\uD1B5\uC801\uC778 \uD30C\uC2F1 \uBC29\uBC95\uC5D0 \uBE44\uD574 XML \uC791\
  \uC5C5\uC744 \uB2E8\uC21C\uD654\uD569\uB2C8\uB2E4; \uC9C1\uC811 XML\uC744 \uAC1D\
  \uCCB4\uB85C \uCE90\uC2A4\uD305\uD558\uBBC0\uB85C\u2026"
lastmod: '2024-04-05T21:53:57.231348-06:00'
model: gpt-4-0125-preview
summary: "XML, \uB610\uB294 eXtensible Markup Language\uB294 90\uB144\uB300 \uB9D0\
  \uBD80\uD130 \uB4F1\uC7A5\uD574 \uAD6C\uC870\uD654\uB41C \uB370\uC774\uD130\uB97C\
  \ \uC704\uD55C \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uB418\uB294 \uD615\uC2DD\
  \uC73C\uB85C \uB0A8\uC544 \uC788\uC2B5\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 방법:
```PowerShell
# XML 파일을 변수에 로딩하기
[xml]$xmlContent = Get-Content '경로\파일명.xml'

# XML 노드 접근하기
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "제목: $($book.title)"
}

# 새로운 XML 요소 생성하기
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# 파일에 XML 다시 저장하기
$xmlContent.Save('경로\업데이트된파일명.xml')
```
출력 예시:
```
제목: PowerShell 프로그래밍
제목: XML 핵심
```

## 심화 탐구
XML, 또는 eXtensible Markup Language는 90년대 말부터 등장해 구조화된 데이터를 위한 광범위하게 사용되는 형식으로 남아 있습니다. PowerShell은 전통적인 파싱 방법에 비해 XML 작업을 단순화합니다; 직접 XML을 객체로 캐스팅하므로 익숙한 점 표기법을 통해 요소와 상호작용할 수 있게 합니다.

XML의 대안으로는 JSON, YAML 또는 맞춤형 데이터 형식이 있습니다. 예를 들어, JSON은 가벼운 성격과 웹 기술 사용의 용이성 때문에 인기를 얻었습니다. 그러나, 네임스페이스, 스키마, 및 XSLT 처리와 같은 XML의 확장 기능은 종종 복잡한 문서나 산업 표준에 더 적합하게 만듭니다.

PowerShell은 .NET Framework의 XML 기능을 사용하여 XML 처리를 합니다. 이는 단순한 읽기-쓰기 작업만이 아니라, 스키마를 사용한 유효성 검사, XPath를 사용한 쿼리, 그리고 PowerShell을 통한 XSLT 변환과 같은 작업도 포함한다는 것을 의미합니다.

## 참고하기
- [W3Schools XML 튜토리얼](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
