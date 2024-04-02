---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:37.040517-07:00
description: "Google Apps Script(GAS)\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC77D\uB294 \uAC83\uC740 Google \uB4DC\uB77C\uC774\uBE0C \uB610\uB294 \uB2E4\uB978\
  \ \uC811\uADFC \uAC00\uB2A5\uD55C \uD074\uB77C\uC6B0\uB4DC \uAE30\uBC18 \uC800\uC7A5\
  \uC18C\uC5D0 \uC800\uC7A5\uB41C \uD30C\uC77C\uB85C\uBD80\uD130 \uD14D\uC2A4\uD2B8\
  \ \uB370\uC774\uD130\uB97C \uC811\uADFC\uD558\uACE0 \uCD94\uCD9C\uD558\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC774\uB7EC\uD55C \uD30C\uC77C\uB4E4\uC744 \uC77D\uC5B4\uC11C GAS\
  \ \uD504\uB85C\uC81D\uD2B8 \uB0B4\uC5D0\uC11C\u2026"
lastmod: '2024-03-13T22:44:54.559666-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script(GAS)\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC77D\uB294 \uAC83\uC740 Google \uB4DC\uB77C\uC774\uBE0C \uB610\uB294 \uB2E4\uB978\
  \ \uC811\uADFC \uAC00\uB2A5\uD55C \uD074\uB77C\uC6B0\uB4DC \uAE30\uBC18 \uC800\uC7A5\
  \uC18C\uC5D0 \uC800\uC7A5\uB41C \uD30C\uC77C\uB85C\uBD80\uD130 \uD14D\uC2A4\uD2B8\
  \ \uB370\uC774\uD130\uB97C \uC811\uADFC\uD558\uACE0 \uCD94\uCD9C\uD558\uB294 \uAC83\
  \uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC774\uB7EC\uD55C \uD30C\uC77C\uB4E4\uC744 \uC77D\uC5B4\uC11C GAS\
  \ \uD504\uB85C\uC81D\uD2B8 \uB0B4\uC5D0\uC11C\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## 무엇 & 왜?

Google Apps Script(GAS)에서 텍스트 파일을 읽는 것은 Google 드라이브 또는 다른 접근 가능한 클라우드 기반 저장소에 저장된 파일로부터 텍스트 데이터를 접근하고 추출하는 것을 포함합니다. 프로그래머들은 종종 이러한 파일들을 읽어서 GAS 프로젝트 내에서 바로 텍스트 데이터를 가져오기, 조작하기, 또는 분석하기 위해 필요로 합니다. 이를 통해 자동화와 구글 제품군과의 통합이 가능해집니다.

## 어떻게:

Google Apps Script로 텍스트 파일을 읽기 시작하려면, 일반적으로 Google 드라이브 API를 사용해야 합니다. 다음은 Google 드라이브에서 파일을 읽는 방법을 보여주는 기본 예시입니다:

```javascript
function readFileContents(fileId) {
  // ID로 Google 드라이브 파일을 가져옵니다
  var file = DriveApp.getFileById(fileId);
  
  // 텍스트로서 blob 데이터를 가져옵니다
  var text = file.getBlob().getDataAsString();
  
  // Google Apps Script 로그에 내용을 로깅합니다
  Logger.log(text);
  return text;
}
```

*로그에서의 샘플 출력:*

```
안녕, 세계! 이것은 테스트 텍스트 파일입니다.
```

이 예시에서, `fileId`는 읽고 싶은 파일의 고유 식별자입니다. `DriveApp` 서비스는 파일을 가져오고, `getDataAsString()`은 그 내용을 문자열로 읽습니다. 그런 다음 이 텍스트를 필요에 따라 조작하거나 사용할 수 있습니다.

## 심층 분석

역사적으로, Google Apps Script와 같은 웹 기반 애플리케이션에서 텍스트 파일을 읽는 것은 브라우저 보안 제한과 JavaScript의 비동기적 특성으로 인해 어려움이 있었습니다. Google Apps Script는 `DriveApp`과 같은 추상화된 서비스를 통해 이를 단순화하며, Google 드라이브 파일과 상호작용하기 위한 고수준 API를 제공합니다.

그러나, Google Apps Script에 의해 부과된 성능 및 실행 시간 제한을 고려하는 것이 중요합니다. 특히 큰 파일을 읽거나 데이터와 복잡한 작업을 수행할 때에는, 더 강력한 백엔드에서 직접 Google 클라우드 서비스를 사용하거나, 파일을 더 다루기 쉬운 조각들로 사전 처리하는 것이 더 효율적일 수 있습니다.

복잡한 파일 처리가 필요하거나 실시간 성능이 중요한 경우에는, Node.js, Python, Go를 지원하는 Google Cloud Functions와 같은 대안이 더 많은 유연성과 계산 자원을 제공할 수 있습니다. 그럼에도 불구하고, Google 생태계 내에서 간단한 작업, 특히 구글 제품과의 통합 및 사용의 용이성이 우선시되는 경우에는, Google Apps Script가 놀랍도록 사용자 친화적인 접근 방식을 제공합니다.
