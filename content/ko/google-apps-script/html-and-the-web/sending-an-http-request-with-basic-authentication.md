---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:40.606693-07:00
description: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC HTTP \uC694\
  \uCCAD\uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC\
  \ \uBE44\uBC00\uBC88\uD638\uB97C \uC694\uCCAD \uD5E4\uB354\uC5D0 \uC778\uCF54\uB529\
  \uD558\uC5EC \uBCF4\uD638\uB41C \uB9AC\uC18C\uC2A4\uC5D0 \uC561\uC138\uC2A4\uD558\
  \uB294 \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC11C\uBC84 \uCE21 \uC778\uC99D\uC744 \uC704\uD574, \uB610\uB294\
  \ \uB370\uC774\uD130 \uAC80\uC0C9\uC774\uB098 \uCF58\uD150\uCE20 \uAC8C\uC2DC\uC640\
  \ \uAC19\uC740 \uAE30\uBCF8 \uC778\uC99D\uC744 \uC694\uAD6C\uD558\uB294 API\uC640\
  \ \uD1B5\uD569\uD558\uAE30 \uC704\uD574 \uC774\u2026"
lastmod: '2024-03-13T22:44:54.529545-06:00'
model: gpt-4-0125-preview
summary: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC HTTP \uC694\uCCAD\
  \uC744 \uBCF4\uB0B4\uB294 \uAC83\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\
  \uBC00\uBC88\uD638\uB97C \uC694\uCCAD \uD5E4\uB354\uC5D0 \uC778\uCF54\uB529\uD558\
  \uC5EC \uBCF4\uD638\uB41C \uB9AC\uC18C\uC2A4\uC5D0 \uC561\uC138\uC2A4\uD558\uB294\
  \ \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC11C\uBC84 \uCE21 \uC778\uC99D\uC744 \uC704\uD574, \uB610\uB294 \uB370\
  \uC774\uD130 \uAC80\uC0C9\uC774\uB098 \uCF58\uD150\uCE20 \uAC8C\uC2DC\uC640 \uAC19\
  \uC740 \uAE30\uBCF8 \uC778\uC99D\uC744 \uC694\uAD6C\uD558\uB294 API\uC640 \uD1B5\
  \uD569\uD558\uAE30 \uC704\uD574 \uC774\u2026"
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD558\uC5EC HTTP \uC694\uCCAD\
  \ \uBCF4\uB0B4\uAE30"
weight: 45
---

## 무엇이며 왜 사용하는가?

기본 인증을 사용하여 HTTP 요청을 보내는 것은 사용자 이름과 비밀번호를 요청 헤더에 인코딩하여 보호된 리소스에 액세스하는 과정을 포함합니다. 프로그래머들은 서버 측 인증을 위해, 또는 데이터 검색이나 콘텐츠 게시와 같은 기본 인증을 요구하는 API와 통합하기 위해 이 방법을 사용합니다.

## 어떻게 사용하는가:

Google Apps Script에서 기본 인증을 사용하여 HTTP 요청을 보내기 위해, `UrlFetchApp` 서비스와 base64로 인코딩된 인증 헤더를 결합하여 사용합니다. 다음은 단계별 안내입니다:

1. **자격 증명 인코딩**: 먼저, 사용자 이름과 비밀번호를 base64로 인코딩합니다. Google Apps Script는 문자열에 대한 네이티브 base64 인코딩 기능이 없으므로, 이 목적을 위해 Utilities.base64Encode를 사용합니다.

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **요청 옵션 설정**: 인코딩된 자격 증명이 준비되면, 메소드와 헤더를 포함한 HTTP 요청을 위한 옵션 객체를 준비합니다.

```javascript
var options = {
  method: 'get', // 혹은 'post', 'put', 너의 필요에 따라
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // 추가적인 옵션들을 여기에 추가할 수 있습니다, 예: 'muteHttpExceptions'로 오류 처리
};
```

3. **요청 실행**: 타겟 URL과 옵션 객체를 사용하여 `UrlFetchApp.fetch` 메소드로 요청을 실행합니다.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

성공적인 요청에 대한 샘플 출력은 API의 응답에 따라 다를 수 있습니다. JSON 기반 API의 경우, 다음과 같은 것을 볼 수 있습니다:

```
{"status":"Success","data":"여기에 리소스 데이터..."}
```

응답 코드를 확인하거나 `muteHttpExceptions` 옵션을 사용하여 더 통제된 오류 관리를 위해 가능한 HTTP 오류를 처리하는 것이 중요합니다.

## 심층 분석

기본 인증을 사용하여 HTTP 요청을 보내는 것은 인증이 필요한 웹 기반 리소스에 액세스하기 위한 많은 프로그래밍 언어에서 표준 방법이 되었습니다. Google Apps Script의 맥락에서, `UrlFetchApp`은 인증을 요구하는 이러한 HTTP 요청을 수행하기 위한 직접적인 방법을 제공합니다. 요청 헤더에 기본 자격 증명을 포함하는 것은 단순하지만 효과적인 방법이지만, 자격 증명이 단순히 base64로 인코딩된 평문으로 전송되므로, 가로챌 경우 쉽게 디코딩될 수 있다는 보안상의 주의점이 있습니다.

보안을 개선하기 위해, 특히 민감한 데이터나 작업을 다룰 때는 OAuth 2.0과 같은 대안이 권장됩니다. Google Apps Script는 이 프로토콜을 지원하는 서비스에 대해 인증하는 과정을 간소화하는 `OAuth2` 라이브러리에 대한 내장 지원을 가지고 있습니다.

기본 인증은 보안 한계에도 불구하고, 넓은 인터넷에 노출되지 않은 간단하거나 내부 애플리케이션에 대해 널리 사용됩니다. 적절한 헤더가 설정된 단일 요청만 필요하기 때문에 구현이 간단하며, 더 높은 보안 방법이 사용할 수 없는 API를 위한 빠른 통합이나 간편한 옵션으로 매력적입니다. 그러나 프로그래머들은 보안 함의를 고려하고 가능할 때마다 더 안전한 대안을 모색하도록 권장됩니다.
