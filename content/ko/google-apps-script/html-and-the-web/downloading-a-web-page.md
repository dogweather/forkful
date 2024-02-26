---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:43.738351-07:00
description: "Google Apps Script\uC5D0\uC11C \uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\
  \uB85C\uB4DC\uB294 HTML\uC744 \uD1B5\uD574 \uC6F9 \uD398\uC774\uC9C0\uC758 \uB0B4\
  \uC6A9\uC744 \uAC00\uC838\uC624\uB294 \uC791\uC5C5\uC73C\uB85C, \uC6F9 \uC2A4\uD06C\
  \uB798\uD551, \uB370\uC774\uD130 \uCD94\uCD9C, \uBCC0\uACBD \uC0AC\uD56D \uAC10\uC2DC\
  \ \uB4F1 \uB2E4\uC591\uD55C \uBAA9\uC801\uC744 \uC704\uD574 \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uC790\
  \uB3D9\uD654\uD558\uC5EC \uB370\uC774\uD130 \uC218\uC9D1\uC774\uB098 \uD1B5\uD569\
  \ \uC791\uC5C5\uC744 \uCD5C\uC18C\uD654\uD558\uACE0 \uC2E4\uC2DC\uAC04\u2026"
lastmod: '2024-02-25T18:49:51.567859-07:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\
  \uB4DC\uB294 HTML\uC744 \uD1B5\uD574 \uC6F9 \uD398\uC774\uC9C0\uC758 \uB0B4\uC6A9\
  \uC744 \uAC00\uC838\uC624\uB294 \uC791\uC5C5\uC73C\uB85C, \uC6F9 \uC2A4\uD06C\uB798\
  \uD551, \uB370\uC774\uD130 \uCD94\uCD9C, \uBCC0\uACBD \uC0AC\uD56D \uAC10\uC2DC\
  \ \uB4F1 \uB2E4\uC591\uD55C \uBAA9\uC801\uC744 \uC704\uD574 \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uC790\
  \uB3D9\uD654\uD558\uC5EC \uB370\uC774\uD130 \uC218\uC9D1\uC774\uB098 \uD1B5\uD569\
  \ \uC791\uC5C5\uC744 \uCD5C\uC18C\uD654\uD558\uACE0 \uC2E4\uC2DC\uAC04\u2026"
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Google Apps Script에서 웹 페이지 다운로드는 HTML을 통해 웹 페이지의 내용을 가져오는 작업으로, 웹 스크래핑, 데이터 추출, 변경 사항 감시 등 다양한 목적을 위해 사용됩니다. 프로그래머들은 이 작업을 자동화하여 데이터 수집이나 통합 작업을 최소화하고 실시간 데이터 처리를 보장하기 위해 이 작업을 선택합니다.

## 방법:

Google Apps Script에서는 `UrlFetchApp` 서비스가 웹 콘텐츠를 다운로드하는 데 중요합니다. 아래는 웹 페이지의 HTML 내용을 가져와 로그에 기록하는 방법과 간단한 예를 단계별로 안내합니다:

1. **기본 Fetch 작업:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- 이 코드는 example.com의 HTML 내용을 가져와 로그에 기록합니다. 추가 매개변수 없이 웹 페이지의 소스를 얻는 간단한 시연입니다.

2. **리다이렉트 및 HTTPS 처리:**

HTTPS 또는 리다이렉트 처리의 경우 코드는 대부분 동일하지만, 리다이렉트에 대한 특정 옵션을 구현하거나 오류 처리를 고려할 수 있습니다:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // 자동으로 리다이렉트 따름
    'muteHttpExceptions': true // 가능한 예외를 음소거하여 우아하게 처리
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **속도 제한 및 할당량:**

Google Apps Script의 할당량을 명심하고, 사용량이 많을 경우 속도 제한에 대한 오류 처리가 필요할 수 있습니다.

## 심층 분석

역사적으로, 웹 콘텐츠 다운로드 및 조작은 간단한 HTTP 요청으로 시작하여 스크립팅 언어의 등장과 함께 크게 발전했습니다. Google Apps Script는 G Suite 생태계 내에서 이러한 작업을 직관적으로 실행할 수 있게 해주며, Google의 견고한 인프라를 활용합니다. `UrlFetchApp` 서비스는 이 기능의 핵심 요소로, 복잡한 HTTP/S 요청을 보다 단순한 애플리케이션 수준 인터페이스로 래핑합니다.

그러나 편리함에도 불구하고, Google Apps Script는 실행 시간 제한과 Google이 부과하는 할당량 때문에 중량급 웹 스크래핑이 필요하거나 데이터의 복잡한 후처리가 요구될 때 항상 최선의 도구는 아닐 수 있습니다. 이러한 경우, Puppeteer 또는 Cheerio와 같은 라이브러리를 갖춘 Node.js와 같은 비동기 I/O 작업용으로 설계된 전용 웹 스크래핑 프레임워크나 언어가 더 많은 유연성과 힘을 제공할 수 있습니다.

또한, Google Apps Script는 Google 서비스(예: Sheets, Docs, Drive)와의 통합 및 가벼운 데이터 가져오기 작업을 수행하기에 탁월한 도구이지만, 실행 환경의 한계를 염두에 두는 것이 중요합니다. 집약적인 작업의 경우, Google Cloud Functions 또는 외부 컴퓨팅 리소스로 처리하기 위한 Apps Script의 고급 서비스를 사용하는 것을 고려하십시오.
