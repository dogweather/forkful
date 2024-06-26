---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:40.426368-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C HTTP \uC694\uCCAD\uC744\
  \ \uBCF4\uB0B4\uB294 \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\uC740 `UrlFetchApp` \uC11C\
  \uBE44\uC2A4\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC774 \uC11C\
  \uBE44\uC2A4\uB294 HTTP GET \uBC0F POST \uC694\uCCAD\uC744 \uB9CC\uB4E4\uAE30 \uC704\
  \uD55C \uBA54\uC18C\uB4DC\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740\
  \ JSON \uB370\uC774\uD130\uB97C \uAC80\uC0C9\uD558\uAE30 \uC704\uD574 GET \uC694\
  \uCCAD\uC744\u2026"
lastmod: '2024-03-13T22:44:54.524014-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uB294\
  \ \uAE30\uBCF8\uC801\uC778 \uBC29\uBC95\uC740 `UrlFetchApp` \uC11C\uBE44\uC2A4\uB97C\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## 방법:
Google Apps Script에서 HTTP 요청을 보내는 기본적인 방법은 `UrlFetchApp` 서비스를 사용하는 것입니다. 이 서비스는 HTTP GET 및 POST 요청을 만들기 위한 메소드를 제공합니다. 다음은 JSON 데이터를 검색하기 위해 GET 요청을 하는 간단한 예입니다:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

서버에 데이터를 보내는 데 흔히 사용되는 POST 요청의 경우, 옵션 매개변수에 더 많은 세부 사항을 포함해야 합니다:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // 자바스크립트 객체를 JSON 문자열로 변환
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

이 스니펫들은 기본적인 GET 및 POST 요청 구현을 보여줍니다. 출력은 API 응답에 따라 달라지며, Google Apps Script의 Logger에서 볼 수 있습니다.

## 심층 탐구
Google Apps Script의 `UrlFetchApp` 서비스는 처음 출시된 이후 상당한 발전을 이루었으며, 헤더 설정, 페이로드, 파일 업로드를 위한 다중부분/폼-데이터 처리와 같은 기능을 통해 HTTP 요청에 대한 더 세밀한 제어를 제공합니다. 외부 웹 서비스를 통합하는 수단을 제공하면서도, 더 강력한 백엔드 언어에서 온 개발자들에게는 Python의 `requests`나 Node.js의 JavaScript `fetch` API 같은 라이브러리에 비해 기능적으로 다소 제한적이라고 느낄 수 있습니다.

주목할 만한 제한 사항 중 하나는 Google Apps Script의 실행 시간 제한으로, 장기 실행 요청에 영향을 줍니다. 또한, `UrlFetchApp`은 다양한 사용 사례를 커버하지만 OAuth 인증이나 매우 큰 페이로드를 처리하는 더 복잡한 시나리오의 경우 창의적인 해결책을 찾거나 추가적인 Google Cloud 자원을 활용해야 할 수도 있습니다.

그럼에도 불구하고 Google Workspace 개발자들이 마주하는 대부분의 통합 작업—데이터 검색 자동화에서부터 외부 서비스에 업데이트 게시에 이르기까지—에 있어서 `UrlFetchApp`은 강력하면서 접근하기 쉬운 도구를 제공합니다. Google Apps Script에 통합되어 있어 외부 라이브러리나 복잡한 설정이 필요 없으므로 Google Apps Script의 제약 내에서 HTTP 요청을 비교적 간단하게 실행할 수 있습니다. 웹 API의 환경이 계속해서 확장됨에 따라, `UrlFetchApp`은 Google Apps Script 프로그램이 Google의 생태계를 넘어 외부 세계와 상호작용하는 중요한 다리 역할을 계속하고 있습니다.
