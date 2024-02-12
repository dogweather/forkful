---
title:                "HTML 분석하기"
aliases:
- /ko/google-apps-script/parsing-html/
date:                  2024-02-01T21:57:33.348924-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 분석하기"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Google Apps Script에서 HTML 파싱은 HTML 콘텐츠에서 데이터를 추출하는 것을 포함하며, 특히 웹 페이지나 웹 기반 데이터 소스와 상호작용할 때 유용합니다. 프로그래머들은 이 작업을 데이터 수집을 자동화하고, 웹 콘텐츠를 조작하거나, Google Apps에 웹 기능을 통합하기 위해 사용합니다.

## 방법:
Google Apps Script는 HTML을 파싱하기 위한 내장 메소드가 없습니다. 그러나, `UrlFetchApp` 서비스를 사용하여 HTML 콘텐츠를 검색한 다음, JavaScript 메소드 또는 정규 표현식(regex)을 사용하여 파싱할 수 있습니다. 아래는 웹페이지의 제목 태그를 가져오고 파싱하는 기본 예제입니다.

```javascript
function parseHTMLTitle(url) {
  // 웹페이지의 HTML 콘텐츠를 가져옵니다
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // 간단한 정규 표현식을 사용하여 <title> 태그의 내용을 찾습니다
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // 제목이 발견되었는지 확인하고 반환합니다
  if (match && match.length > 1) {
    return match[1];
  }

  return '제목을 찾을 수 없습니다';
}

// 사용 예시
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // 웹페이지의 제목을 출력합니다
```

보다 정교한 HTML 파싱을 위해, `XmlService`를 사용하여 HTML을 XML로 파싱할 수 있습니다. 하지만, 이 방법은 HTML이 잘 구성된 XML이어야 한다는 요구사항이 있으며, 항상 그런 것은 아닙니다:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // 여기서부터 XmlService 메소드를 사용하여 XML 트리를 탐색합니다
    // 예를 들어, 특정 엘리먼트나 속성을 찾는 등
  } catch(e) {
    Logger.log('HTML 파싱 중 오류: ' + e.toString());
  }
}
```

## 심층 분석:
역사적으로, Google Apps Script와 같은 환경에서 HTML 파싱은 Document Object Model (DOM)이나 다른 프로그래밍 맥락에서 흔한 전용 파싱 라이브러리가 부족하여 어려웠습니다. 예를 들어, 브라우저의 JavaScript는 DOM을 쉽게 사용할 수 있고, Node.js 환경은 `cheerio`나 `jsdom` 같은 수많은 NPM 패키지에 접근할 수 있어 HTML을 파싱합니다.

Google Apps Script의 접근 방식은 `UrlFetchApp`을 사용하여 웹 요청을 하고 정규 표현식 또는 XML 파싱 메소드를 사용하여 응답 데이터를 조작하는 데 주로 기반을 두고 있습니다. 정규 표현식은 간단한 파싱 작업에 유용할 수 있지만, 오류의 위험과 코드의 취약성 때문에 복잡한 HTML에 대해서는 일반적으로 권장되지 않습니다. `XmlService`를 사용한 XML 파싱은 더 구조화된 접근 방식을 제공하지만, 임의의 웹 페이지를 다룰 때 제한이 될 수 있는 잘 구성된 HTML/XML이 필요합니다.

복잡한 파싱 요구 사항이 있거나 형식이 잘못된 HTML을 다루는 경우, Google Apps Script 외부의 웹 서비스를 사용하는 대안 전략이 있을 수 있습니다. 이 서비스는 HTML 콘텐츠를 처리할 수 있는 더 강력한 파싱 기술이나 라이브러리를 사용할 수 있으며, Google Apps Script가 쉽게 사용할 수 있는 형식으로 처리된 데이터를 반환할 수 있습니다. 하지만, 이 접근 방식은 네트워크 지연과 추가 웹 서비스 관리의 복잡성을 도입합니다.

이러한 도전에도 불구하고, Google Apps Script 내에서 HTML을 파싱하는 것은 특히 다른 Google 서비스 및 API와 결합될 때 강력한 도구로 남아 있으며, 생산성과 데이터 처리 능력을 크게 향상시킬 수 있는 다양한 자동화 가능성을 제공합니다.
